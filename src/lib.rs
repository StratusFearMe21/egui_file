#![feature(iter_collect_into)]

use egui::{
  text::CCursor, text_edit::TextEditState, vec2, Align2, Context, Key, Layout, Pos2, RichText,
  ScrollArea, TextEdit, Ui, Vec2, Window,
};
use fst::{
  automaton::{AlwaysMatch, StartsWith, Str},
  raw::Fst,
  Automaton, Error, IntoStreamer, Streamer,
};
use std::{
  borrow::Cow,
  env,
  fmt::Debug,
  fs::{self, ReadDir},
  path::{Path, PathBuf},
};

macro_rules! seperator {
  () => {
    if cfg!(not(windows)) {
      '/'
    } else {
      '\\'
    }
  };
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
/// Dialog state.
pub enum State {
  /// Is open.
  Open,
  /// Was closed.
  Closed,
  /// Was canceled.
  Cancelled,
  /// File was selected.
  Selected,
}

enum ReadDirState {
  Loading(ReadDir),
  Fst(usize, fst::raw::Builder<Vec<u8>>),
  Done,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
/// Dialog type.
pub enum DialogType {
  SelectFolder,
  OpenFile,
  SaveFile,
}

struct CompleterState {
  folder_depth: usize,
  current_tooltip: Option<Cow<'static, str>>,
  machine: Fst<Vec<u8>>,
  too_large: bool,
}

/// `egui` component that represents `OpenFileDialog` or `SaveFileDialog`.
pub struct FileDialog {
  /// Current opened path.
  path: PathBuf,
  /// Editable field with path.
  path_edit: String,

  /// Selected file path
  selected_file: Option<String>,
  /// Editable field with filename.
  filename_edit: String,

  /// Files in directory.
  files: Result<Vec<String>, Error>,
  /// The file completer
  completion: CompleterState,
  /// Current dialog state.
  state: State,
  /// Dialog type.
  dialog_type: DialogType,

  /// File reading state
  reading_state: ReadDirState,

  current_pos: Option<Pos2>,
  scrollarea_max_height: f32,
  anchor: Option<(Align2, Vec2)>,
  filter: Option<Filter>,
  resizable: bool,
  rename: bool,
  new_folder: bool,

  // Show hidden files on unix systems.
  #[cfg(unix)]
  show_hidden: bool,
  #[cfg(unix)]
  hidden_files: usize,
}

impl Debug for FileDialog {
  #[cfg(target_family = "unix")]
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("FileDialog")
      .field("path", &self.path)
      .field("path_edit", &self.path_edit)
      .field("selected_file", &self.selected_file)
      .field("filename_edit", &self.filename_edit)
      .field("files", &self.files)
      .field("state", &self.state)
      .field("dialog_type", &self.dialog_type)
      .field("current_pos", &self.current_pos)
      .field("scrollarea_max_height", &self.scrollarea_max_height)
      .field("anchor", &self.anchor)
      // Closures don't implement std::fmt::Debug.
      // .field("filter", &self.filter)
      .field("resizable", &self.resizable)
      .field("rename", &self.rename)
      .field("new_folder", &self.new_folder)
      .field("show_hidden", &self.show_hidden)
      .finish()
  }

  #[cfg(not(target_family = "unix"))]
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("FileDialog")
      .field("path", &self.path)
      .field("path_edit", &self.path_edit)
      .field("selected_file", &self.selected_file)
      .field("filename_edit", &self.filename_edit)
      .field("files", &self.files)
      .field("state", &self.state)
      .field("dialog_type", &self.dialog_type)
      .field("current_pos", &self.current_pos)
      .field("scrollarea_max_height", &self.scrollarea_max_height)
      .field("anchor", &self.anchor)
      // Closures don't implement std::fmt::Debug.
      // .field("filter", &self.filter)
      .field("resizable", &self.resizable)
      .field("rename", &self.rename)
      .field("new_folder", &self.new_folder)
      .finish()
  }
}

/// Function that returns `true` if the path is accepted.
pub type Filter = fn(&Path) -> bool;

impl FileDialog {
  /// Create dialog that prompts the user to select a folder.
  pub fn select_folder(initial_path: Option<PathBuf>) -> Self {
    FileDialog::new(DialogType::SelectFolder, initial_path, None)
  }

  /// Create dialog that prompts the user to open a file.
  pub fn open_file(initial_path: Option<PathBuf>, filter: Option<Filter>) -> Self {
    FileDialog::new(DialogType::OpenFile, initial_path, filter)
  }

  /// Create dialog that prompts the user to save a file.
  pub fn save_file(initial_path: Option<PathBuf>, filter: Option<Filter>) -> Self {
    FileDialog::new(DialogType::SaveFile, initial_path, filter)
  }

  /// Constructs new file dialog. If no `initial_path` is passed,`env::current_dir` is used.
  fn new(dialog_type: DialogType, initial_path: Option<PathBuf>, filter: Option<Filter>) -> Self {
    let mut path = initial_path.unwrap_or_else(|| env::current_dir().unwrap_or_default());
    let mut filename_edit = String::new();

    if path.is_file() {
      assert!(dialog_type != DialogType::SelectFolder);
      filename_edit = get_file_name(&path).to_string();
      path.pop();
    }

    let mut path_edit = path.to_str().unwrap_or_default().to_string();
    if path.is_dir() {
      path_edit.push(seperator!());
    }

    let mut s = Self {
      completion: CompleterState {
        folder_depth: { memchr::memchr_iter(seperator!() as u8, path_edit.as_bytes()).count() },
        current_tooltip: None,
        machine: Fst::from_iter_set([[]]).unwrap(),
        too_large: false,
      },
      path,
      path_edit,
      selected_file: None,
      filename_edit,
      files: Ok(Vec::new()),
      reading_state: ReadDirState::Done,
      state: State::Closed,
      dialog_type,
      current_pos: None,
      scrollarea_max_height: 320.0,
      anchor: None,
      filter,
      resizable: true,
      rename: true,
      new_folder: true,

      #[cfg(unix)]
      show_hidden: false,
      #[cfg(unix)]
      hidden_files: 0,
    };

    s.refresh();

    s
  }

  /// Set the window anchor.
  pub fn anchor(mut self, align: Align2, offset: impl Into<Vec2>) -> Self {
    self.anchor = Some((align, offset.into()));
    self
  }

  /// Set the window position.
  pub fn current_pos(mut self, current_pos: impl Into<Pos2>) -> Self {
    self.current_pos = Some(current_pos.into());
    self
  }

  /// Set the maximum height for the inner [ScrollArea]
  pub fn scrollarea_max_height(mut self, max: f32) -> Self {
    self.scrollarea_max_height = max;
    self
  }

  /// Enable/disable resizing the window. Default is `true`.
  pub fn resizable(mut self, resizable: bool) -> Self {
    self.resizable = resizable;
    self
  }

  /// Show the Rename button. Default is `true`.
  pub fn show_rename(mut self, rename: bool) -> Self {
    self.rename = rename;
    self
  }

  /// Show the New Folder button. Default is `true`.
  pub fn show_new_folder(mut self, new_folder: bool) -> Self {
    self.new_folder = new_folder;
    self
  }

  /// Set a function to filter shown files.
  pub fn filter(mut self, filter: Filter) -> Self {
    self.filter = Some(filter);
    self.refresh();
    self
  }

  /// Get the dialog type.
  pub fn dialog_type(&self) -> DialogType {
    self.dialog_type
  }

  /// Get the window's visibility.
  pub fn visible(&self) -> bool {
    self.state == State::Open
  }

  /// Opens the dialog.
  pub fn open(&mut self) {
    self.state = State::Open;
  }

  /// Resulting file path.
  pub fn path(&self) -> Option<PathBuf> {
    self.selected_file.as_ref().map(|f| self.path.join(f))
  }

  /// Dialog state.
  pub fn state(&self) -> State {
    self.state
  }

  /// Returns true, if the file selection was confirmed.
  pub fn selected(&self) -> bool {
    self.state == State::Selected
  }

  fn open_selected(&mut self) {
    if let Some(path) = &self.selected_file {
      if path.ends_with(seperator!()) {
        self.path = self.path.join(path);
        self.refresh();
      } else if self.dialog_type == DialogType::OpenFile {
        self.confirm();
      }
    }
  }

  fn confirm(&mut self) {
    self.state = State::Selected;
  }

  fn read_dir_into(&mut self) -> usize {
    if let ReadDirState::Loading(ref mut paths) = self.reading_state {
      let iter = paths
        .filter_map(|result| result.ok())
        .filter(|entry| {
          if entry.path().is_dir() {
            return true;
          }

          if self.dialog_type == DialogType::SelectFolder {
            return false;
          }

          if !entry.path().is_file() {
            return false;
          }

          // Filter.
          if let Some(filter) = self.filter {
            filter(&entry.path())
          } else {
            println!("No filter");
            false
          }
        })
        .map(|entry| {
          let mut file_name = entry.file_name().into_string().unwrap();
          #[cfg(unix)]
          if file_name.starts_with('.') {
            self.hidden_files += 1;
          }
          if entry.path().is_dir() {
            #[cfg(not(windows))]
            file_name.push('/');
            #[cfg(windows)]
            file_name.push('\\');
          }
          file_name
        });

      if let Ok(ref mut files) = self.files {
        let current_len = files.len();
        iter.take(5000).collect_into(files);
        let new_len = files.len();

        new_len - current_len
      } else {
        0
      }
    } else {
      0
    }
  }

  fn refresh(&mut self) {
    #[cfg(unix)]
    self.hidden_files = 0;
    match fs::read_dir(&self.path) {
      Ok(paths) => {
        self.files = Ok(Vec::new());
        self.reading_state = ReadDirState::Loading(paths);
        let read = self.read_dir_into();

        if let Ok(ref mut files) = self.files {
          files.sort_unstable();

          if read < 5000 {
            self.completion.machine = Fst::from_iter_set(files.iter()).unwrap();

            self.reading_state = ReadDirState::Done;
          }
        }
      }
      Err(e) => self.files = Err(fst::Error::Io(e)),
    }
    self.completion.folder_depth =
      memchr::memchr_iter(seperator!() as u8, self.path_edit.as_bytes()).count();
    self.path_edit = String::from(self.path.to_str().unwrap_or_default());
    self.select(None);
  }

  fn read_more(&mut self) {
    match &mut self.reading_state {
      ReadDirState::Done => {}

      ReadDirState::Loading(_) => {
        if self.read_dir_into() < 5000 {
          if let Ok(ref mut files) = self.files {
            files.sort_unstable();

            self.reading_state = ReadDirState::Fst(0, fst::raw::Builder::memory());
          }
        }
      }

      ReadDirState::Fst(pos, builder) => {
        if let Ok(ref files) = self.files {
          if (*pos + 500) < files.len() {
            files[*pos..*pos + 500]
              .iter()
              .for_each(|f| builder.add(f).unwrap());

            *pos += 500;
          } else {
            files[*pos..].iter().for_each(|f| builder.add(f).unwrap());

            let mut builder_buf = fst::raw::Builder::memory();
            std::mem::swap(&mut builder_buf, builder);
            self.completion.machine = builder_buf.into_fst();

            self.reading_state = ReadDirState::Done;
          }
        } else {
          self.reading_state = ReadDirState::Done;
        }
      }
    }
  }

  fn select(&mut self, file: Option<String>) {
    self.filename_edit = match &file {
      Some(path) => path.clone(),
      None => String::new(),
    };
    self.selected_file = file;
  }

  fn can_save(&self) -> bool {
    self.selected_file.is_some() || !self.filename_edit.is_empty()
  }

  fn can_open(&self) -> bool {
    self.selected_file.is_some()
  }

  fn can_rename(&self) -> bool {
    if !self.filename_edit.is_empty() {
      if let Some(file) = &self.selected_file {
        return self.filename_edit.ne(file);
      }
    }
    false
  }

  fn title(&self) -> &str {
    match self.dialog_type {
      DialogType::SelectFolder => "📁  Select Folder",
      DialogType::OpenFile => "📂  Open File",
      DialogType::SaveFile => "💾  Save File",
    }
  }

  /// Shows the dialog if it is open. It is also responsible for state management.
  /// Should be called every ui update.
  pub fn show(&mut self, ctx: &Context) -> &Self {
    self.state = match self.state {
      State::Open => {
        if ctx.input(|i| i.key_pressed(Key::Escape)) {
          self.state = State::Cancelled;
        }

        let mut is_open = true;
        self.ui(ctx, &mut is_open);
        match is_open {
          true => self.state,
          false => State::Closed,
        }
      }
      _ => State::Closed,
    };

    self
  }

  fn ui(&mut self, ctx: &Context, is_open: &mut bool) {
    let mut window = Window::new(RichText::new(self.title()).strong())
      .open(is_open)
      .resizable(self.resizable)
      .collapsible(false);

    if let Some((align, offset)) = self.anchor {
      window = window.anchor(align, offset);
    }

    if let Some(current_pos) = self.current_pos {
      window = window.current_pos(current_pos);
    }

    window.show(ctx, |ui| self.ui_in_window(ui));
  }

  fn ui_in_window(&mut self, ui: &mut Ui) {
    enum Command {
      Cancel,
      CreateDirectory,
      Folder,
      Open(PathBuf),
      OpenSelected,
      Refresh,
      Rename(PathBuf, PathBuf),
      Save(PathBuf),
      Select(String),
      UpDirectory,
    }
    let mut command: Option<Command> = None;
    self.read_more();

    // Top directory field with buttons.
    ui.horizontal(|ui| {
      ui.add_enabled_ui(self.path.parent().is_some(), |ui| {
        let response = ui.button("⬆").on_hover_text_at_pointer("Parent Folder");
        if response.clicked() {
          command = Some(Command::UpDirectory);
        }
      });
      ui.with_layout(Layout::right_to_left(egui::Align::Center), |ui| {
        ui.add_enabled_ui(matches!(self.reading_state, ReadDirState::Done), |ui| {
          let response = ui.button("⟲").on_hover_text_at_pointer("Refresh");
          if response.clicked() {
            command = Some(Command::Refresh);
          }
        });

        let mut response = ui.add_sized(
          ui.available_size(),
          TextEdit::singleline(&mut self.path_edit),
        );
        if let Some(ref tooltip) = self.completion.current_tooltip {
          egui::containers::show_tooltip_for(
            ui.ctx(),
            response.id.with("__tooltip"),
            &response.rect,
            |ui| ui.label(tooltip.as_ref()),
          );
        }
        if response.has_focus() {
          ui.memory_mut(|m| m.lock_focus(response.id, true));
          if ui.input(|i| i.key_pressed(Key::Tab)) {
            if let Some(mut text_state) = TextEditState::load(ui.ctx(), response.id) {
              text_state.set_ccursor_range(None);
              text_state.store(ui.ctx(), response.id);
            }
            response.mark_changed();
          }
        }
        'fst: {
          if response.changed() {
            let current_path = Path::new(&self.path_edit);
            let file_name = current_path
              .file_name()
              .unwrap_or_default()
              .to_str()
              .unwrap_or_default();

            let depth = memchr::memchr_iter(seperator!() as u8, self.path_edit.as_bytes()).count();
            if depth != self.completion.folder_depth {
              if let Ok(paths) = fs::read_dir(
                if depth > self.completion.folder_depth || self.path_edit.ends_with(seperator!()) {
                  &current_path
                } else {
                  current_path.parent().unwrap_or_else(
                    #[cfg(windows)]
                    || Path::new("C:\\"),
                    #[cfg(not(windows))]
                    || Path::new("/"),
                  )
                },
              ) {
                let mut paths: Vec<String> = paths
                  .filter_map(|result| result.ok())
                  .filter(|entry| {
                    if entry.path().is_dir() {
                      return true;
                    }

                    if self.dialog_type == DialogType::SelectFolder {
                      return false;
                    }

                    if !entry.path().is_file() {
                      return false;
                    }

                    // Filter.
                    if let Some(filter) = self.filter {
                      filter(&entry.path())
                    } else {
                      println!("No filter");
                      false
                    }
                  })
                  .filter_map(|entry| {
                    let mut file_name = entry.file_name().into_string().unwrap();
                    #[cfg(unix)]
                    if file_name.starts_with('.') {
                      return None;
                    }
                    if entry.path().is_dir() {
                      #[cfg(not(windows))]
                      file_name.push('/');
                      #[cfg(windows)]
                      file_name.push('\\');
                    }
                    Some(file_name)
                  })
                  .take(1000)
                  .collect();

                if paths.len() == 1000 {
                  self.completion.current_tooltip =
                    Some(Cow::Borrowed("Directory too large for completion"));
                  self.completion.too_large = true;
                } else {
                  paths.sort_unstable();
                  self.completion.too_large = false;
                  self.completion.machine = Fst::from_iter_set(paths.iter()).unwrap();
                }
              } else {
                break 'fst;
              }
              self.completion.folder_depth = depth;
            }

            enum StreamType<'f> {
              StartsWith(fst::raw::Stream<'f, StartsWith<Str<'f>>>),
              Root(fst::raw::Stream<'f, AlwaysMatch>),
            }

            impl<'a, 'f> Streamer<'a> for StreamType<'f> {
              type Item = &'a [u8];

              fn next(&'a mut self) -> Option<Self::Item> {
                match self {
                  StreamType::StartsWith(s) => s.next().map(|f| f.0),
                  StreamType::Root(r) => r.next().map(|f| f.0),
                }
              }

              fn next_start_node(&'a self) -> Option<fst::raw::Node<'_>> {
                match self {
                  StreamType::StartsWith(s) => s.next_start_node(),
                  StreamType::Root(r) => r.next_start_node(),
                }
              }
            }

            if !self.completion.too_large {
              let matcher = fst::automaton::Str::new(file_name).starts_with();
              let backspace = ui.input(|i| i.key_pressed(Key::Backspace));
              let mut stream = if self.path_edit.ends_with(seperator!()) || backspace {
                StreamType::Root(self.completion.machine.stream())
              } else {
                StreamType::StartsWith(
                  self
                    .completion
                    .machine
                    .search(matcher)
                    .gt(file_name)
                    .into_stream(),
                )
              };

              if let Some(mut node) = stream.next_start_node() {
                if node.len() == 1 && !node.is_final() && !backspace {
                  let ccursor_start = self.path_edit.len();
                  while node.len() == 1 {
                    let t = node.transitions().next().unwrap();
                    self.path_edit.push(t.inp as char);
                    node = self.completion.machine.node(t.addr);
                  }
                  let ccursor_end = self.path_edit.len();

                  if let Some(mut text_state) = TextEditState::load(ui.ctx(), response.id) {
                    text_state.set_ccursor_range(Some(egui::text_edit::CCursorRange {
                      primary: CCursor {
                        index: ccursor_start,
                        prefer_next_row: true,
                      },
                      secondary: CCursor {
                        index: ccursor_end,
                        prefer_next_row: true,
                      },
                    }));

                    text_state.store(ui.ctx(), response.id);
                  }

                  self.completion.current_tooltip = None;
                } else {
                  let mut tooltip = String::new();
                  let mut num_processed = 0;
                  while let Some(key) = stream.next() {
                    if num_processed > 500 {
                      break;
                    }
                    let key = unsafe { std::str::from_utf8_unchecked(key) };
                    #[cfg(unix)]
                    if !self.show_hidden && key.starts_with('.') {
                      continue;
                    }
                    std::fmt::Write::write_fmt(
                      &mut tooltip,
                      format_args!(
                        "{}{}\n",
                        if key.ends_with(seperator!()) {
                          "🗀 "
                        } else {
                          "🗋 "
                        },
                        key
                      ),
                    )
                    .unwrap();
                    num_processed += 1;
                  }
                  if num_processed > 500 {
                    tooltip.push('…');
                  } else {
                    tooltip.pop();
                  }
                  if !tooltip.is_empty() {
                    self.completion.current_tooltip = Some(Cow::Owned(tooltip));
                  } else {
                    self.completion.current_tooltip = None;
                  }
                }
              } else {
                break 'fst;
              }
            }
          }
          if response.lost_focus() {
            self.completion.current_tooltip = None;
            let current_path = Path::new(&self.path_edit);
            command = Some(Command::Open(current_path.to_path_buf()));
          };
        }
      });
    });

    // Rows with files.
    ui.separator();

    let row_height = ui.spacing().interact_size.y;

    ScrollArea::vertical()
      .max_height(self.scrollarea_max_height)
      .show_rows(
        ui,
        row_height,
        #[cfg(unix)]
        self.files.as_ref().map_or(1, |f| {
          if self.show_hidden {
            f.len()
          } else {
            f.len() - self.hidden_files
          }
        }),
        #[cfg(not(unix))]
        self.files.as_ref().map_or(1, |f| f.len()),
        |ui, #[cfg(not(unix))] range, #[cfg(unix)] mut range| match &self.files {
          Ok(files) => {
            #[cfg(unix)]
            if !self.show_hidden {
              range.start += self.hidden_files;
              range.end += self.hidden_files;
            }
            for path in files[range].iter() {
              let is_dir = path.ends_with(seperator!());

              #[cfg(unix)]
              if !self.show_hidden && path.starts_with('.') {
                continue;
              }

              ui.with_layout(ui.layout().with_cross_justify(true), |ui| {
                let label = match is_dir {
                  true => "🗀 ",
                  false => "🗋 ",
                }
                .to_string()
                  + path;

                let is_selected = Some(path) == self.selected_file.as_ref();
                let selectable_label = ui.selectable_label(is_selected, label);
                if selectable_label.clicked() {
                  command = Some(Command::Select(path.clone()));
                }

                if selectable_label.double_clicked() {
                  command = Some(match self.dialog_type == DialogType::SaveFile {
                    true => match is_dir {
                      true => Command::OpenSelected,
                      false => Command::Save(self.path.join(path)),
                    },
                    false => Command::Open(self.path.join(path)),
                  });
                }
              });
            }
          }
          Err(e) => {
            ui.label(e.to_string());
          }
        },
      );

    // Bottom file field.
    ui.separator();
    ui.horizontal(|ui| {
      ui.label("File:");
      ui.with_layout(Layout::right_to_left(egui::Align::Center), |ui| {
        if self.new_folder && ui.button("New Folder").clicked() {
          command = Some(Command::CreateDirectory);
        }

        if self.rename {
          ui.add_enabled_ui(self.can_rename(), |ui| {
            if ui.button("Rename").clicked() {
              if let Some(ref from) = self.selected_file {
                let from = Path::new(from);
                let to = from.with_file_name(&self.filename_edit);
                command = Some(Command::Rename(from.to_path_buf(), to));
              }
            }
          });
        }

        let result = ui.add_sized(
          ui.available_size(),
          TextEdit::singleline(&mut self.filename_edit),
        );

        if result.lost_focus()
          && result.ctx.input(|i| i.key_pressed(egui::Key::Enter))
          && !self.filename_edit.is_empty()
        {
          let mut path = self.path.join(&self.filename_edit);
          match self.dialog_type {
            DialogType::SelectFolder => {
              command = Some(Command::Folder);
            }
            DialogType::OpenFile => {
              if path.exists() {
                path.pop();
                command = Some(Command::Open(path));
              }
            }
            DialogType::SaveFile => {
              command = Some(match path.is_dir() {
                true => {
                  path.pop();
                  Command::Open(path)
                }
                false => Command::Save(path),
              });
            }
          }
        }
      });
    });

    // Confirm, Cancel buttons.
    ui.horizontal(|ui| {
      match self.dialog_type {
        DialogType::SelectFolder => {
          let should_open = match &self.selected_file {
            Some(file) => file.ends_with(seperator!()),
            None => true,
          };

          ui.horizontal(|ui| {
            ui.set_enabled(should_open);
            if ui.button("Open").clicked() {
              command = Some(Command::Folder);
            };
          });
        }
        DialogType::OpenFile => {
          ui.horizontal(|ui| {
            ui.set_enabled(self.can_open());
            if ui.button("Open").clicked() {
              command = Some(Command::OpenSelected);
            };
          });
        }
        DialogType::SaveFile => {
          let should_open_directory = match &self.selected_file {
            Some(file) => file.ends_with(seperator!()),
            None => false,
          };

          if should_open_directory {
            if ui.button("Open").clicked() {
              command = Some(Command::OpenSelected);
            };
          } else {
            ui.horizontal(|ui| {
              ui.set_enabled(self.can_save());
              if ui.button("Save").clicked() {
                let filename = &self.filename_edit;
                let path = self.path.join(filename);
                command = Some(Command::Save(path));
              };
            });
          }
        }
      }

      if ui.button("Cancel").clicked() {
        command = Some(Command::Cancel);
      }

      #[cfg(unix)]
      ui.with_layout(Layout::right_to_left(egui::Align::Center), |ui| {
        ui.checkbox(&mut self.show_hidden, "Show Hidden");
      });
    });

    if let Some(command) = command {
      match command {
        Command::Select(file) => {
          self.select(Some(file));
        }
        Command::Folder => {
          let path = self.path.join(&self.filename_edit);
          self.selected_file = Some(match path.is_dir() {
            true => path.file_name().unwrap().to_str().unwrap().to_string(),
            false => self
              .path
              .clone()
              .file_name()
              .unwrap()
              .to_str()
              .unwrap()
              .to_string(),
          });
          self.confirm();
        }
        Command::Open(mut path) => {
          let mut file_name = path.file_name().unwrap().to_str().unwrap().to_string();

          if path.is_dir() {
            file_name.push(seperator!());
          }

          path.pop();
          self.path = path;
          self.select(Some(file_name));
          self.open_selected();
        }
        Command::OpenSelected => {
          self.open_selected();
        }
        Command::Save(file) => {
          self.selected_file = Some(file.file_name().unwrap().to_str().unwrap().to_string());
          self.confirm();
        }
        Command::Cancel => {
          self.state = State::Cancelled;
        }
        Command::Refresh => {
          self.refresh();
        }
        Command::UpDirectory => {
          if self.path.pop() {
            self.refresh();
            #[cfg(not(windows))]
            self.path_edit.push('/');
            #[cfg(windows)]
            self.path_edit.push('\\');
          }
        }
        Command::CreateDirectory => {
          let mut path = self.path.clone();
          let name = match self.filename_edit.is_empty() {
            true => "New folder",
            false => &self.filename_edit,
          };
          path.push(name);
          match fs::create_dir(&path) {
            Ok(_) => {
              self.refresh();
              self.select(Some(
                path.file_name().unwrap().to_str().unwrap().to_string()
                  + if cfg!(windows) { "\\" } else { "/" },
              ));
              // TODO: scroll to selected?
            }
            Err(err) => println!("Error while creating directory: {err}"),
          }
        }
        Command::Rename(from, to) => match fs::rename(from, &to) {
          Ok(_) => {
            self.refresh();
            // self.select(Some(to));
          }
          Err(err) => println!("Error while renaming: {err}"),
        },
      };
    }
  }
}

#[cfg(windows)]
fn is_drive_root(path: &Path) -> bool {
  if let Some(path) = path.to_str() {
    if let Some(ch) = path.chars().next() {
      return ('A'..='Z').contains(&ch) && &path[1..] == ":\\";
    }
  }
  false
}

fn get_file_name(path: &Path) -> &str {
  match path.is_dir() {
    #[cfg(windows)]
    true if is_drive_root(path) => path.to_str().unwrap_or_default(),
    _ => path
      .file_name()
      .and_then(|name| name.to_str())
      .unwrap_or_default(),
  }
}

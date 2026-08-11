//! Application state shared across commands.
//!
//! Tauri hands every command a reference to whatever was passed to
//! `Builder::manage`, so a single `AppState` is the idiomatic place to keep
//! anything that must outlive one IPC call. Interior mutability is required
//! because commands only ever get an immutable borrow.

use std::sync::atomic::AtomicBool;
use std::sync::Mutex;

use serde::{Deserialize, Serialize};

/// The default hotkey registered at startup. Users can change it at runtime.
pub const DEFAULT_SHORTCUT: &str = "CmdOrCtrl+Shift+D";

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Note {
    pub id: u64,
    pub title: String,
    pub body: String,
    pub done: bool,
}

#[derive(Debug, Clone, Copy, Default, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct NoteStats {
    pub total: usize,
    pub done: usize,
    pub open: usize,
}

/// An in-memory note store. Kept free of Tauri types so it can be unit tested
/// without booting an application instance.
#[derive(Debug, Default)]
pub struct Notes {
    items: Vec<Note>,
    next_id: u64,
}

impl Notes {
    pub fn add(&mut self, title: &str, body: &str) -> Result<Note, String> {
        let title = title.trim();
        if title.is_empty() {
            return Err("a note needs a title".into());
        }
        if self.items.iter().any(|n| n.title.eq_ignore_ascii_case(title)) {
            return Err(format!("a note titled {title:?} already exists"));
        }
        self.next_id += 1;
        let note = Note {
            id: self.next_id,
            title: title.to_owned(),
            body: body.trim().to_owned(),
            done: false,
        };
        self.items.push(note.clone());
        Ok(note)
    }

    pub fn list(&self) -> Vec<Note> {
        self.items.clone()
    }

    pub fn toggle(&mut self, id: u64) -> Result<Note, String> {
        let note = self
            .items
            .iter_mut()
            .find(|n| n.id == id)
            .ok_or_else(|| format!("no note with id {id}"))?;
        note.done = !note.done;
        Ok(note.clone())
    }

    pub fn remove(&mut self, id: u64) -> Result<Note, String> {
        let idx = self
            .items
            .iter()
            .position(|n| n.id == id)
            .ok_or_else(|| format!("no note with id {id}"))?;
        Ok(self.items.remove(idx))
    }

    pub fn stats(&self) -> NoteStats {
        let done = self.items.iter().filter(|n| n.done).count();
        NoteStats {
            total: self.items.len(),
            done,
            open: self.items.len() - done,
        }
    }
}

pub struct AppState {
    pub notes: Mutex<Notes>,
    /// Set by `cancel_job` and polled by the streaming job command.
    pub job_cancelled: AtomicBool,
    /// When true, closing the window hides it instead of quitting.
    pub close_to_tray: AtomicBool,
    /// The currently registered global shortcut, as an accelerator string.
    pub shortcut: Mutex<String>,
}

impl Default for AppState {
    fn default() -> Self {
        Self {
            notes: Mutex::new(Notes::default()),
            job_cancelled: AtomicBool::new(false),
            close_to_tray: AtomicBool::new(false),
            shortcut: Mutex::new(DEFAULT_SHORTCUT.to_owned()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn add_assigns_increasing_ids_and_trims() {
        let mut notes = Notes::default();
        let first = notes.add("  buy milk  ", " 2 litres ").unwrap();
        let second = notes.add("call vet", "").unwrap();

        assert_eq!(first.id, 1);
        assert_eq!(first.title, "buy milk");
        assert_eq!(first.body, "2 litres");
        assert_eq!(second.id, 2);
        assert!(!first.done);
    }

    #[test]
    fn add_rejects_blank_titles() {
        let mut notes = Notes::default();
        assert!(notes.add("   ", "body").is_err());
        assert!(notes.list().is_empty());
    }

    #[test]
    fn add_rejects_case_insensitive_duplicates() {
        let mut notes = Notes::default();
        notes.add("Buy Milk", "").unwrap();
        let err = notes.add("buy milk", "").unwrap_err();
        assert!(err.contains("already exists"));
        assert_eq!(notes.list().len(), 1);
    }

    #[test]
    fn ids_are_not_reused_after_removal() {
        let mut notes = Notes::default();
        let first = notes.add("one", "").unwrap();
        notes.remove(first.id).unwrap();
        let second = notes.add("two", "").unwrap();
        assert_eq!(second.id, 2);
    }

    #[test]
    fn toggle_flips_done_and_reports_missing_ids() {
        let mut notes = Notes::default();
        let note = notes.add("one", "").unwrap();
        assert!(notes.toggle(note.id).unwrap().done);
        assert!(!notes.toggle(note.id).unwrap().done);
        assert!(notes.toggle(999).is_err());
    }

    #[test]
    fn remove_returns_the_removed_note() {
        let mut notes = Notes::default();
        notes.add("one", "").unwrap();
        let two = notes.add("two", "").unwrap();
        assert_eq!(notes.remove(two.id).unwrap().title, "two");
        assert_eq!(notes.list().len(), 1);
        assert!(notes.remove(two.id).is_err());
    }

    #[test]
    fn stats_split_open_and_done() {
        let mut notes = Notes::default();
        let a = notes.add("a", "").unwrap();
        notes.add("b", "").unwrap();
        notes.add("c", "").unwrap();
        notes.toggle(a.id).unwrap();

        let stats = notes.stats();
        assert_eq!(stats.total, 3);
        assert_eq!(stats.done, 1);
        assert_eq!(stats.open, 2);
    }
}

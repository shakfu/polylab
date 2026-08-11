//! Every function marked `#[tauri::command]` is callable from the frontend
//! with `invoke("name", { args })`.
//!
//! Three shapes are demonstrated here:
//!   * plain synchronous commands (run on the main thread, keep them cheap),
//!   * `async` commands (run on Tauri's async runtime, safe to await in),
//!   * a command that streams progress back over an `ipc::Channel`.

use std::path::{Path, PathBuf};
use std::sync::atomic::Ordering;
use std::time::Duration;

use serde::{Deserialize, Serialize};
use tauri::ipc::Channel;
use tauri::{AppHandle, Manager, State};

use crate::state::{AppState, Note, NoteStats};

/// Anything that is `Serialize` can cross the IPC boundary. `serde`'s
/// `rename_all` keeps Rust snake_case fields idiomatic on both sides.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Greeting {
    pub message: String,
    pub greeted_at_ms: u128,
    pub from: String,
}

#[tauri::command]
pub fn greet(name: &str) -> Result<Greeting, String> {
    let name = name.trim();
    if name.is_empty() {
        // Returning `Err` rejects the promise on the JS side.
        return Err("please enter a name first".into());
    }
    Ok(Greeting {
        message: format!("Hello, {name}! This string was built in Rust."),
        greeted_at_ms: std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_millis())
            .unwrap_or_default(),
        from: format!("{} on {}", std::env::consts::ARCH, std::env::consts::OS),
    })
}

// --- Managed state -------------------------------------------------------
// `State<AppState>` is injected by Tauri; the argument never appears in the
// JS call site.

#[tauri::command]
pub fn add_note(state: State<'_, AppState>, title: String, body: String) -> Result<Note, String> {
    let mut notes = state.notes.lock().map_err(|e| e.to_string())?;
    notes.add(&title, &body)
}

#[tauri::command]
pub fn list_notes(state: State<'_, AppState>) -> Result<Vec<Note>, String> {
    let notes = state.notes.lock().map_err(|e| e.to_string())?;
    Ok(notes.list())
}

#[tauri::command]
pub fn toggle_note(state: State<'_, AppState>, id: u64) -> Result<Note, String> {
    let mut notes = state.notes.lock().map_err(|e| e.to_string())?;
    notes.toggle(id)
}

#[tauri::command]
pub fn delete_note(state: State<'_, AppState>, id: u64) -> Result<Note, String> {
    let mut notes = state.notes.lock().map_err(|e| e.to_string())?;
    notes.remove(id)
}

#[tauri::command]
pub fn note_stats(state: State<'_, AppState>) -> Result<NoteStats, String> {
    let notes = state.notes.lock().map_err(|e| e.to_string())?;
    Ok(notes.stats())
}

// --- Filesystem ----------------------------------------------------------
// Rust-side file access is not restricted by the frontend capability scope,
// which is why reading an arbitrary path the user picked in a dialog belongs
// in a command rather than in the `fs` plugin.

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct FilePreview {
    pub path: String,
    pub name: String,
    pub size_bytes: u64,
    pub is_utf8: bool,
    pub line_count: usize,
    pub preview: String,
}

#[tauri::command]
pub async fn read_file_preview(path: String, max_bytes: usize) -> Result<FilePreview, String> {
    let path = PathBuf::from(path);
    let meta = std::fs::metadata(&path).map_err(|e| format!("cannot stat file: {e}"))?;
    if meta.is_dir() {
        return Err("that is a directory, not a file".into());
    }

    let bytes = std::fs::read(&path).map_err(|e| format!("cannot read file: {e}"))?;
    let head = &bytes[..bytes.len().min(max_bytes.max(1))];
    let (is_utf8, preview) = match std::str::from_utf8(head) {
        Ok(text) => (true, text.to_owned()),
        Err(_) => (
            false,
            head.iter()
                .map(|b| format!("{b:02x}"))
                .collect::<Vec<_>>()
                .join(" "),
        ),
    };

    Ok(FilePreview {
        name: file_name_of(&path),
        path: path.to_string_lossy().into_owned(),
        size_bytes: meta.len(),
        is_utf8,
        line_count: if is_utf8 { preview.lines().count() } else { 0 },
        preview,
    })
}

#[tauri::command]
pub async fn write_text_file(path: String, contents: String) -> Result<u64, String> {
    let path = PathBuf::from(path);
    std::fs::write(&path, contents.as_bytes()).map_err(|e| format!("cannot write file: {e}"))?;
    Ok(contents.len() as u64)
}

fn file_name_of(path: &Path) -> String {
    path.file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_else(|| path.to_string_lossy().into_owned())
}

// --- Streaming work over a channel --------------------------------------
// `Channel<T>` is the cheapest way to push many messages back to the caller of
// a single command. An externally tagged enum gives the frontend a
// discriminated union to switch on.

#[derive(Clone, Serialize)]
#[serde(rename_all = "camelCase", tag = "kind")]
pub enum JobEvent {
    #[serde(rename_all = "camelCase")]
    Started { steps: u32 },
    #[serde(rename_all = "camelCase")]
    Progress {
        step: u32,
        steps: u32,
        percent: u32,
        note: String,
    },
    #[serde(rename_all = "camelCase")]
    Finished { steps: u32, checksum: String },
    #[serde(rename_all = "camelCase")]
    Cancelled { at_step: u32 },
}

#[tauri::command]
pub async fn run_job(
    state: State<'_, AppState>,
    steps: u32,
    step_ms: u64,
    on_event: Channel<JobEvent>,
) -> Result<String, String> {
    let steps = steps.clamp(1, 500);
    let step_ms = step_ms.clamp(10, 2_000);
    state.job_cancelled.store(false, Ordering::SeqCst);
    on_event
        .send(JobEvent::Started { steps })
        .map_err(|e| e.to_string())?;

    // A 64-bit FNV-1a hash over the step numbers: cheap, deterministic work
    // that stands in for whatever the real job would be.
    let mut hash: u64 = 0xcbf2_9ce4_8422_2325;
    for step in 1..=steps {
        if state.job_cancelled.load(Ordering::SeqCst) {
            on_event
                .send(JobEvent::Cancelled { at_step: step })
                .map_err(|e| e.to_string())?;
            return Ok(format!("cancelled at step {step}"));
        }

        tokio::time::sleep(Duration::from_millis(step_ms)).await;
        hash ^= u64::from(step);
        hash = hash.wrapping_mul(0x0000_0100_0000_01b3);

        on_event
            .send(JobEvent::Progress {
                step,
                steps,
                percent: step * 100 / steps,
                note: format!("hashed step {step}"),
            })
            .map_err(|e| e.to_string())?;
    }

    let checksum = format!("{hash:016x}");
    on_event
        .send(JobEvent::Finished {
            steps,
            checksum: checksum.clone(),
        })
        .map_err(|e| e.to_string())?;
    Ok(checksum)
}

#[tauri::command]
pub fn cancel_job(state: State<'_, AppState>) {
    state.job_cancelled.store(true, Ordering::SeqCst);
}

// --- Misc app plumbing ---------------------------------------------------

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct AppInfo {
    pub name: String,
    pub version: String,
    pub tauri_version: String,
    pub identifier: String,
    pub app_data_dir: String,
    pub debug_build: bool,
}

#[tauri::command]
pub fn app_info(app: AppHandle) -> Result<AppInfo, String> {
    let config = app.config();
    Ok(AppInfo {
        name: config.product_name.clone().unwrap_or_default(),
        version: config.version.clone().unwrap_or_default(),
        tauri_version: tauri::VERSION.to_owned(),
        identifier: config.identifier.clone(),
        app_data_dir: app
            .path()
            .app_data_dir()
            .map(|p| p.to_string_lossy().into_owned())
            .map_err(|e| e.to_string())?,
        debug_build: cfg!(debug_assertions),
    })
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TrayPrefs {
    pub close_to_tray: bool,
}

#[tauri::command]
pub fn set_close_to_tray(state: State<'_, AppState>, prefs: TrayPrefs) -> bool {
    state
        .close_to_tray
        .store(prefs.close_to_tray, Ordering::SeqCst);
    prefs.close_to_tray
}

/// Deliberately fallible, so the frontend has something to catch.
#[tauri::command]
pub fn divide(numerator: f64, denominator: f64) -> Result<f64, String> {
    if denominator == 0.0 {
        return Err("cannot divide by zero -- this Err became a rejected promise".into());
    }
    Ok(numerator / denominator)
}

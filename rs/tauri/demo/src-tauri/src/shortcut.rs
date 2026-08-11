//! Global (system-wide) keyboard shortcut.
//!
//! Desktop only -- the plugin does not exist on mobile targets, so both this
//! module and its dependency are compiled out there.

use std::str::FromStr;

use tauri::{AppHandle, Emitter, Manager, State};
use tauri_plugin_global_shortcut::{GlobalShortcutExt, Shortcut, ShortcutState};

use crate::state::AppState;

pub const SHORTCUT_EVENT: &str = "shortcut://pressed";

/// Installs the plugin and registers the shortcut currently held in state.
pub fn init(app: &AppHandle) -> tauri::Result<()> {
    app.plugin(
        tauri_plugin_global_shortcut::Builder::new()
            .with_handler(|app, shortcut, event| {
                // The handler fires for press and release; only react once.
                if event.state() != ShortcutState::Pressed {
                    return;
                }
                if let Some(window) = app.get_webview_window("main") {
                    let _ = window.show();
                    let _ = window.unminimize();
                    let _ = window.set_focus();
                }
                let _ = app.emit(SHORTCUT_EVENT, shortcut.into_string());
            })
            .build(),
    )?;

    let state = app.state::<AppState>();
    let accelerator = state.shortcut.lock().map_or_else(
        |_| crate::state::DEFAULT_SHORTCUT.to_owned(),
        |s| s.clone(),
    );
    // A shortcut can be owned by another application; that is not fatal.
    if let Err(err) = register(app, &accelerator) {
        eprintln!("could not register global shortcut {accelerator}: {err}");
    }
    Ok(())
}

fn register(app: &AppHandle, accelerator: &str) -> Result<(), String> {
    let shortcut = Shortcut::from_str(accelerator)
        .map_err(|e| format!("{accelerator:?} is not a valid accelerator: {e}"))?;
    app.global_shortcut()
        .register(shortcut)
        .map_err(|e| e.to_string())
}

/// Swaps the registered hotkey, rolling back to the previous one on failure.
#[tauri::command]
pub fn set_global_shortcut(
    app: AppHandle,
    state: State<'_, AppState>,
    accelerator: String,
) -> Result<String, String> {
    let accelerator = accelerator.trim().to_owned();
    let mut current = state.shortcut.lock().map_err(|e| e.to_string())?;
    if accelerator == *current {
        return Ok(accelerator);
    }

    let previous = current.clone();
    if let Ok(shortcut) = Shortcut::from_str(&previous) {
        let _ = app.global_shortcut().unregister(shortcut);
    }

    match register(&app, &accelerator) {
        Ok(()) => {
            *current = accelerator.clone();
            Ok(accelerator)
        }
        Err(err) => {
            let _ = register(&app, &previous);
            Err(err)
        }
    }
}

#[tauri::command]
pub fn global_shortcut(state: State<'_, AppState>) -> Result<String, String> {
    state.shortcut.lock().map(|s| s.clone()).map_err(|e| e.to_string())
}

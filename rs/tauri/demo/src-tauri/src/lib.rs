mod commands;
mod state;

#[cfg(desktop)]
mod menu;
#[cfg(desktop)]
mod shortcut;
#[cfg(desktop)]
mod tray;

use std::sync::atomic::Ordering;
use std::time::Duration;

use serde::Serialize;
use tauri::{Emitter, Listener, Manager, WindowEvent};

use state::AppState;

/// Pushed to every window once a second by a background task.
#[derive(Clone, Serialize)]
#[serde(rename_all = "camelCase")]
struct Tick {
    seq: u64,
    uptime_secs: u64,
    open_notes: usize,
}

const TICK_EVENT: &str = "app://tick";
/// The frontend emits this; Rust listens and logs it.
const UI_LOG_EVENT: &str = "ui://log";

#[cfg_attr(mobile, tauri::mobile_entry_point)]
pub fn run() {
    let mut builder = tauri::Builder::default()
        .plugin(tauri_plugin_opener::init())
        .plugin(tauri_plugin_dialog::init())
        .plugin(tauri_plugin_fs::init())
        .plugin(tauri_plugin_os::init())
        .plugin(tauri_plugin_notification::init())
        .plugin(tauri_plugin_clipboard_manager::init())
        .plugin(tauri_plugin_store::Builder::new().build())
        .manage(AppState::default());

    // Command handlers differ per platform, so the list is assembled in two
    // steps rather than one `generate_handler!` call.
    builder = builder.invoke_handler(tauri::generate_handler![
        commands::greet,
        commands::add_note,
        commands::list_notes,
        commands::toggle_note,
        commands::delete_note,
        commands::note_stats,
        commands::read_file_preview,
        commands::write_text_file,
        commands::run_job,
        commands::cancel_job,
        commands::app_info,
        commands::set_close_to_tray,
        commands::divide,
        #[cfg(desktop)]
        shortcut::set_global_shortcut,
        #[cfg(desktop)]
        shortcut::global_shortcut,
    ]);

    builder
        .setup(|app| {
            let handle = app.handle().clone();

            #[cfg(desktop)]
            {
                app.set_menu(menu::build(&handle)?)?;
                app.on_menu_event(|app, event| menu::on_event(app, event.id().as_ref()));
                tray::build(&handle)?;
                shortcut::init(&handle)?;
            }

            // Frontend -> Rust: `emit("ui://log", payload)` in the webview
            // lands here. Useful for telemetry-style one-way messages.
            app.listen(UI_LOG_EVENT, |event| {
                println!("[ui] {}", event.payload());
            });

            // Rust -> frontend: a periodic push, the counterpart of a command.
            tauri::async_runtime::spawn(async move {
                let mut seq = 0u64;
                loop {
                    tokio::time::sleep(Duration::from_secs(1)).await;
                    seq += 1;
                    let open_notes = handle
                        .state::<AppState>()
                        .notes
                        .lock()
                        .map(|n| n.stats().open)
                        .unwrap_or(0);
                    if handle
                        .emit(
                            TICK_EVENT,
                            Tick {
                                seq,
                                uptime_secs: seq,
                                open_notes,
                            },
                        )
                        .is_err()
                    {
                        break;
                    }
                }
            });

            Ok(())
        })
        .on_window_event(|window, event| {
            // Intercepting the close request is how "minimise to tray"
            // behaviour is implemented. The tray menu still offers Quit.
            if let WindowEvent::CloseRequested { api, .. } = event {
                let state = window.app_handle().state::<AppState>();
                if state.close_to_tray.load(Ordering::SeqCst) {
                    api.prevent_close();
                    let _ = window.hide();
                }
            }
        })
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}

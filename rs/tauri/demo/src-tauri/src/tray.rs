//! System tray icon with its own menu.
//!
//! Requires the `tray-icon` and `image-png` features on the `tauri` crate.

use tauri::menu::{MenuBuilder, MenuItemBuilder};
use tauri::tray::{MouseButton, MouseButtonState, TrayIconBuilder, TrayIconEvent};
use tauri::{AppHandle, Emitter, Manager};
use tauri_plugin_notification::NotificationExt;

use crate::menu::MENU_EVENT;

pub fn build(app: &AppHandle) -> tauri::Result<()> {
    let show = MenuItemBuilder::with_id("tray:show", "Show window").build(app)?;
    let notify = MenuItemBuilder::with_id("tray:notify", "Send a notification").build(app)?;
    let new_note = MenuItemBuilder::with_id("menu:new-note", "New note").build(app)?;
    let quit = MenuItemBuilder::with_id("tray:quit", "Quit").build(app)?;
    let menu = MenuBuilder::new(app)
        .items(&[&show, &new_note, &notify])
        .separator()
        .items(&[&quit])
        .build()?;

    TrayIconBuilder::with_id("main-tray")
        .tooltip("Tauri feature demo")
        .icon(
            app.default_window_icon()
                .cloned()
                .expect("the bundle config always provides a default window icon"),
        )
        .menu(&menu)
        // Left click should reveal the window; the menu stays on right click.
        .show_menu_on_left_click(false)
        .on_menu_event(|app, event| match event.id().as_ref() {
            "tray:show" => show_main_window(app),
            "tray:notify" => {
                let _ = app
                    .notification()
                    .builder()
                    .title("Tauri feature demo")
                    .body("This notification was sent from the tray menu, in Rust.")
                    .show();
            }
            "tray:quit" => app.exit(0),
            other => {
                let _ = app.emit(MENU_EVENT, other);
                show_main_window(app);
            }
        })
        .on_tray_icon_event(|tray, event| {
            if let TrayIconEvent::Click {
                button: MouseButton::Left,
                button_state: MouseButtonState::Up,
                ..
            } = event
            {
                show_main_window(tray.app_handle());
            }
        })
        .build(app)?;

    Ok(())
}

fn show_main_window(app: &AppHandle) {
    if let Some(window) = app.get_webview_window("main") {
        let _ = window.show();
        let _ = window.unminimize();
        let _ = window.set_focus();
    }
}

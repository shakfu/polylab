//! Native application menu.
//!
//! Setting a custom menu replaces Tauri's default one entirely, which on macOS
//! also removes the standard Edit shortcuts (Cmd+C/V/A). The Edit submenu below
//! puts them back -- text inputs in the webview stop working properly without
//! it, and that is an easy detail to miss.

use tauri::menu::{
    AboutMetadata, CheckMenuItemBuilder, Menu, MenuBuilder, MenuItemBuilder, SubmenuBuilder,
};
use tauri::{AppHandle, Emitter, Manager, Runtime};

/// Emitted to the frontend whenever a custom menu item is chosen.
pub const MENU_EVENT: &str = "menu://action";

pub fn build<R: Runtime>(app: &AppHandle<R>) -> tauri::Result<Menu<R>> {
    let app_menu = SubmenuBuilder::new(app, "Demo")
        .about(Some(AboutMetadata {
            name: Some("Tauri Feature Demo".into()),
            version: Some(app.package_info().version.to_string()),
            comments: Some("A tour of the Tauri v2 application APIs.".into()),
            ..Default::default()
        }))
        .separator()
        .services()
        .separator()
        .hide()
        .hide_others()
        .show_all()
        .separator()
        .quit()
        .build()?;

    // Predefined items are wired to the webview by the OS; no handler needed.
    let edit_menu = SubmenuBuilder::new(app, "Edit")
        .undo()
        .redo()
        .separator()
        .cut()
        .copy()
        .paste()
        .select_all()
        .build()?;

    let demo_menu = SubmenuBuilder::new(app, "Actions")
        .item(
            &MenuItemBuilder::with_id("menu:new-note", "New note")
                .accelerator("CmdOrCtrl+N")
                .build(app)?,
        )
        .item(
            &MenuItemBuilder::with_id("menu:run-job", "Run background job")
                .accelerator("CmdOrCtrl+R")
                .build(app)?,
        )
        .separator()
        .item(
            &CheckMenuItemBuilder::with_id("menu:always-on-top", "Always on top")
                .checked(false)
                .build(app)?,
        )
        .separator()
        .item(&MenuItemBuilder::with_id("menu:docs", "Tauri documentation").build(app)?)
        .build()?;

    let window_menu = SubmenuBuilder::new(app, "Window")
        .minimize()
        .maximize()
        .separator()
        .close_window()
        .build()?;

    MenuBuilder::new(app)
        .items(&[&app_menu, &edit_menu, &demo_menu, &window_menu])
        .build()
}

/// Forwards custom menu clicks to the frontend as an event. Keeping the menu
/// dumb and letting the UI decide what to do avoids duplicating logic in Rust.
pub fn on_event<R: Runtime>(app: &AppHandle<R>, id: &str) {
    match id {
        "menu:new-note" | "menu:run-job" | "menu:always-on-top" | "menu:docs" => {
            if let Some(window) = app.get_webview_window("main") {
                let _ = window.show();
                let _ = window.set_focus();
            }
            let _ = app.emit(MENU_EVENT, id);
        }
        _ => {}
    }
}

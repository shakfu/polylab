# Tauri feature demo

A single-window Tauri v2 application built on the vanilla TypeScript template. Each panel in the
UI exercises one part of the Tauri surface area, and the code behind it is short enough to read in
one sitting.

```
make install     # bun install
make dev         # bun run tauri dev
make test        # tsc --noEmit + cargo test
make build       # production bundle
```

## What each panel shows

**Commands and state.** `invoke` calls a `#[tauri::command]` and gets back a serialised struct.
A command returning `Result::Err` rejects the JS promise, so ordinary `try/catch` handles failure.
The note list is the interesting one: the notes live in a `Mutex<Notes>` registered with
`Builder::manage`, the frontend keeps no copy, and every mutation is followed by a re-read. That is
the usual shape for state that must outlive a single IPC call.

**Events and streaming.** Three different transports, each suited to a different job:

- A background task started in `setup()` emits `app://tick` once a second. Events are one-way and
  broadcast to every window, which makes them right for push updates and wrong for request/response.
- The webview emits `ui://log`; Rust subscribes with `listen` and prints it to the terminal.
- `run_job` takes an `ipc::Channel` and sends one message per step from a single `invoke`. The Rust
  enum is `#[serde(tag = "kind")]`, so the frontend switches over a discriminated union. Cancelling
  is a second command that flips an `AtomicBool` the job polls between steps.

**Files and dialogs.** The dialog plugin returns a path and nothing more; the file is read by a Rust
command. This split matters: Rust is not subject to the frontend's filesystem scope, so
"open whatever the user picked" belongs there. The scratchpad below it goes the other way and uses
the `fs` plugin directly from JS, which works only because the capability file grants read and write
under `$APPDATA`.

**Windows.** Minimise, maximise, fullscreen, always-on-top, centre, and retitle the current window,
plus opening a second webview window. Every one of those calls maps to an individual permission --
without `core:window:allow-set-fullscreen` the button throws rather than silently doing nothing.
The panel also explains the close-to-tray interception described below.

**OS integration.** Host details from the `os` plugin, notifications (which need a runtime permission
grant, as on the web), clipboard read and write, and handing URLs to the system browser through the
opener plugin.

**Settings and shortcuts.** The store plugin persists settings as JSON in the app data directory and
writes them back automatically, so the display name, theme, and toggles survive a restart. The global
shortcut is registered in Rust and works while the app is in the background; re-registering it from
the UI rolls back to the previous combination if the new one is already taken by another application.

## Rust-side pieces without a panel

- **Native menu** (`src/menu.rs`). Setting a custom menu replaces the default one entirely, which on
  macOS also removes the standard Edit shortcuts -- the Edit submenu puts Cmd+C/V/A back. Custom
  items emit `menu://action` with their id and let the frontend decide what to do.
- **Tray icon** (`src/tray.rs`). Its own menu, a left-click handler that reveals the window, and a
  Quit item. Needs the `tray-icon` and `image-png` features on the `tauri` crate.
- **Close interception** (`src/lib.rs`). When close-to-tray is enabled, `WindowEvent::CloseRequested`
  is intercepted with `api.prevent_close()` and the window is hidden instead. The tray menu is then
  the only way out, which is why it always offers Quit.
- **Global shortcut** (`src/shortcut.rs`). Desktop only, so both the module and the dependency are
  compiled out on mobile targets.

## Permissions

`src-tauri/capabilities/default.json` is the allow-list. Anything not named there fails at the IPC
boundary with a message naming the missing permission -- `os:default`, for instance, deliberately
omits `os:allow-hostname`. Unknown identifiers are a build error rather than a runtime surprise.

## Layout

```
src/
  main.ts            bootstrap, tab switching, app-wide event listeners
  lib/               DOM helpers and the on-screen event console
  panels/            one module per panel
src-tauri/src/
  lib.rs             builder setup, plugins, background ticker, window events
  commands.rs        every #[tauri::command]
  state.rs           managed state and its unit tests
  menu.rs tray.rs shortcut.rs
```

import { emit, listen } from "@tauri-apps/api/event";
import { openUrl } from "@tauri-apps/plugin-opener";

import { initConsole, log } from "./lib/console";
import { $, errText, setOutput } from "./lib/dom";
import { focusNoteForm, initCommandsPanel } from "./panels/commands";
import { initEventsPanel, type Tick } from "./panels/events";
import { initFilesPanel } from "./panels/files";
import { initSettingsPanel, verboseTicks } from "./panels/settings";
import { initSystemPanel } from "./panels/system";
import { initWindowPanel } from "./panels/windows";

window.addEventListener("DOMContentLoaded", () => {
  bootstrap().catch((error) => {
    // A failure here leaves the UI half-initialised, so make it loud in both
    // the webview console and the terminal running `tauri dev`.
    console.error("bootstrap failed", error);
    log("app", `bootstrap failed: ${errText(error)}`, "err");
    void emit("ui://log", { level: "error", message: `bootstrap failed: ${errText(error)}` });
  });
});

async function bootstrap(): Promise<void> {
  initConsole();
  initNav();

  initCommandsPanel();
  initFilesPanel();
  initWindowPanel();
  await initEventsPanel();

  await registerAppListeners();

  const info = await initSystemPanel();
  $("#app-summary").textContent =
    `${info.identifier} - tauri ${info.tauriVersion} - ${info.debugBuild ? "debug" : "release"} build`;

  await initSettingsPanel();

  log("app", "frontend ready", "ok");
  // One-way notification to the Rust side, printed by the `ui://log` listener.
  await emit("ui://log", { message: "frontend booted", at: new Date().toISOString() });
}

function initNav(): void {
  const buttons = document.querySelectorAll<HTMLButtonElement>(".nav-item");
  for (const button of buttons) {
    button.addEventListener("click", () => showPanel(button.dataset.panel ?? "commands"));
  }
}

function showPanel(name: string): void {
  for (const button of document.querySelectorAll<HTMLButtonElement>(".nav-item")) {
    button.classList.toggle("is-active", button.dataset.panel === name);
  }
  for (const panel of document.querySelectorAll<HTMLElement>(".panel")) {
    panel.classList.toggle("is-active", panel.id === `panel-${name}`);
  }
}

async function registerAppListeners(): Promise<void> {
  const uptime = $("#stat-uptime");
  const openNotes = $("#stat-open");

  await listen<Tick>("app://tick", (event) => {
    uptime.textContent = `${event.payload.uptimeSecs}s`;
    openNotes.textContent = String(event.payload.openNotes);
    if (verboseTicks()) {
      log("event/app://tick", `seq ${event.payload.seq}`);
    }
  });

  // Native menu and tray items are deliberately dumb: they emit an id and the
  // frontend decides what it means.
  await listen<string>("menu://action", async (event) => {
    const id = event.payload;
    setOutput($("#menu-out"), `received ${id} at ${new Date().toLocaleTimeString()}`, "ok");
    log("event/menu", id, "ok");

    switch (id) {
      case "menu:new-note":
        showPanel("commands");
        focusNoteForm();
        break;
      case "menu:run-job":
        showPanel("events");
        $<HTMLButtonElement>("#job-start").click();
        break;
      case "menu:always-on-top":
        showPanel("window");
        $<HTMLButtonElement>("#win-ontop").click();
        break;
      case "menu:docs":
        await openUrl("https://tauri.app/start/");
        break;
    }
  });

  await listen<string>("shortcut://pressed", (event) => {
    log("event/shortcut", `${event.payload} pressed`, "ok");
  });

  window.addEventListener("unhandledrejection", (event) => {
    log("unhandled", errText(event.reason), "err");
  });
}

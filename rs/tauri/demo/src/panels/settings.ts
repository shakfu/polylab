import { invoke } from "@tauri-apps/api/core";
import { load, type Store } from "@tauri-apps/plugin-store";

import { log } from "../lib/console";
import { $, errText } from "../lib/dom";
import { showCloseBehaviour } from "./windows";

interface Settings {
  displayName: string;
  theme: "system" | "dark" | "light";
  closeToTray: boolean;
  verboseTicks: boolean;
}

const DEFAULTS: Settings = {
  displayName: "",
  theme: "system",
  closeToTray: false,
  verboseTicks: false,
};

let settings: Settings = { ...DEFAULTS };
let store: Store | null = null;

export function verboseTicks(): boolean {
  return settings.verboseTicks;
}

export async function initSettingsPanel(): Promise<Settings> {
  // `autoSave` debounces writes to disk; without it, call `store.save()`.
  store = await load("settings.json", { autoSave: 200 });
  settings = {
    displayName: (await store.get<string>("displayName")) ?? DEFAULTS.displayName,
    theme: (await store.get<Settings["theme"]>("theme")) ?? DEFAULTS.theme,
    closeToTray: (await store.get<boolean>("closeToTray")) ?? DEFAULTS.closeToTray,
    verboseTicks: (await store.get<boolean>("verboseTicks")) ?? DEFAULTS.verboseTicks,
  };

  applyTheme(settings.theme);
  await syncCloseToTray(settings.closeToTray);
  bindControls();
  await initShortcut();

  $("#settings-status").textContent = "loaded from settings.json in the app data directory";
  return settings;
}

function bindControls(): void {
  const name = $<HTMLInputElement>("#set-name");
  const theme = $<HTMLSelectElement>("#set-theme");
  const tray = $<HTMLInputElement>("#set-tray");
  const verbose = $<HTMLInputElement>("#set-verbose");

  name.value = settings.displayName;
  theme.value = settings.theme;
  tray.checked = settings.closeToTray;
  verbose.checked = settings.verboseTicks;

  name.addEventListener("change", () => void update("displayName", name.value));

  theme.addEventListener("change", () => {
    const value = theme.value as Settings["theme"];
    applyTheme(value);
    void update("theme", value);
  });

  tray.addEventListener("change", async () => {
    await syncCloseToTray(tray.checked);
    await update("closeToTray", tray.checked);
  });

  verbose.addEventListener("change", () => void update("verboseTicks", verbose.checked));
}

async function update<K extends keyof Settings>(key: K, value: Settings[K]): Promise<void> {
  settings = { ...settings, [key]: value };
  await store?.set(key, value);
  $("#settings-status").textContent = `${key} saved to settings.json in the app data directory`;
  log("store/set", `${key} = ${JSON.stringify(value)}`, "ok");
}

function applyTheme(theme: Settings["theme"]): void {
  document.documentElement.dataset.theme = theme;
}

/** Rust owns the close-to-tray behaviour, so the value has to be pushed over. */
async function syncCloseToTray(enabled: boolean): Promise<void> {
  await invoke<boolean>("set_close_to_tray", { prefs: { closeToTray: enabled } });
  showCloseBehaviour(enabled);
}

async function initShortcut(): Promise<void> {
  const input = $<HTMLInputElement>("#shortcut-input");
  const status = $("#shortcut-status");

  try {
    input.value = await invoke<string>("global_shortcut");
    status.textContent = `${input.value} is registered system-wide`;
  } catch (error) {
    status.textContent = errText(error);
  }

  $("#shortcut-form").addEventListener("submit", async (event) => {
    event.preventDefault();
    try {
      const accelerator = await invoke<string>("set_global_shortcut", {
        accelerator: input.value,
      });
      status.textContent = `${accelerator} is registered system-wide`;
      log("shortcut/register", accelerator, "ok");
    } catch (error) {
      status.textContent = `not registered: ${errText(error)}`;
      log("shortcut/register", errText(error), "err");
    }
  });
}

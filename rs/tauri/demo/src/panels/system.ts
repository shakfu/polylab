import { invoke } from "@tauri-apps/api/core";
import { readText, writeText } from "@tauri-apps/plugin-clipboard-manager";
import {
  isPermissionGranted,
  requestPermission,
  sendNotification,
} from "@tauri-apps/plugin-notification";
import { openUrl } from "@tauri-apps/plugin-opener";
// Namespace import: the plugin exports a function called `type`, which does not
// survive a named import cleanly.
import * as os from "@tauri-apps/plugin-os";

import { log } from "../lib/console";
import { $, errText, setOutput } from "../lib/dom";

export interface AppInfo {
  name: string;
  version: string;
  tauriVersion: string;
  identifier: string;
  appDataDir: string;
  debugBuild: boolean;
}

export async function initSystemPanel(): Promise<AppInfo> {
  const info = await invoke<AppInfo>("app_info");
  await renderInfo(info);
  initNotifications();
  initClipboard();
  initOpener();
  return info;
}

async function renderInfo(info: AppInfo): Promise<void> {
  // Most `os` functions are synchronous because the values are injected at
  // startup; only locale and hostname need a round trip.
  const rows: Array<[string, string]> = [
    ["product", `${info.name} ${info.version} (${info.debugBuild ? "debug" : "release"})`],
    ["identifier", info.identifier],
    ["tauri", info.tauriVersion],
    ["platform", `${os.platform()} / ${os.type()} / ${os.family()}`],
    ["os version", os.version()],
    ["arch", os.arch()],
    ["hostname", (await os.hostname()) ?? "unknown"],
    ["locale", (await os.locale()) ?? "unknown"],
    ["app data", info.appDataDir],
  ];

  const dl = $("#sysinfo");
  dl.innerHTML = "";
  for (const [key, value] of rows) {
    const dt = document.createElement("dt");
    dt.textContent = key;
    const dd = document.createElement("dd");
    dd.textContent = value;
    dl.append(dt, dd);
  }
}

function initNotifications(): void {
  const status = $("#notify-status");

  const check = async (): Promise<boolean> => {
    let granted = await isPermissionGranted();
    if (!granted) {
      granted = (await requestPermission()) === "granted";
    }
    status.textContent = granted
      ? "permission granted"
      : "permission denied -- enable notifications for this app in system settings";
    return granted;
  };

  $("#notify-perm").addEventListener("click", () => void check());

  $("#notify").addEventListener("click", async () => {
    if (!(await check())) return;
    sendNotification({
      title: "Tauri feature demo",
      body: "Sent from the webview through the notification plugin.",
    });
    log("notification/send", "notification dispatched", "ok");
  });
}

function initClipboard(): void {
  const input = $<HTMLInputElement>("#clip-input");
  const out = $("#clip-out");

  $("#clip-write").addEventListener("click", async () => {
    const text = input.value || `copied at ${new Date().toLocaleTimeString()}`;
    await writeText(text);
    setOutput(out, `wrote to the system clipboard:\n${text}`, "ok");
    log("clipboard/write", text, "ok");
  });

  $("#clip-read").addEventListener("click", async () => {
    try {
      setOutput(out, `clipboard now holds:\n${await readText()}`);
    } catch (error) {
      setOutput(out, errText(error), "err");
    }
  });
}

function initOpener(): void {
  const links: Array<[string, string]> = [
    ["#open-docs", "https://tauri.app/start/"],
    ["#open-api", "https://tauri.app/reference/javascript/api/"],
  ];
  for (const [selector, url] of links) {
    $(selector).addEventListener("click", async () => {
      await openUrl(url);
      log("opener/openUrl", url);
    });
  }
}

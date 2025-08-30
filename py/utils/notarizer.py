#!/usr/bin/env python3
"""notarizer

Based on notarizer.sh by Angelo Scicolone
https://github.com/ascicolone/notarizer
"""

import argparse
import os
import subprocess
import sys
from pathlib import Path
from textwrap import dedent
from typing import Optional, TypeAlias

Pathlike: TypeAlias = str | Path


class BundleNotarizer:
    """Codesigns and notarizes MacOS bundles

    Requires a valid Developer ID certificate to be installed in the keychain to 
    codesign the app, and App Store Connect credentials stored in a keychain profile
    to notarize the app.

    To discover names and teams IDs of currently installed Developer Identities,
    just use:

        security find-identity -p basic -v

    To store credentials just use:

        xcrun notarytool store-credentials --apple-id <id> --team-id <id>

    Note that this will ask for (app-specific) app store connect password and
    for a name to save a profile, that you can later use. 
    """

    def __init__(
        self,
        bundle_path: Pathlike,
        developer_identity: Optional[str] = None,
        keychain_profile: Optional[str] = None,
    ):
        self.bundle_path = Path(bundle_path)
        if not bundle_path.exists():
            self.print_colored(
                f"Error: App path '{bundle_path}' does not exist", "bright_red"
            )
            sys.exit(1)
        self.developer_identity = developer_identity or os.getenv("DEV_ID")
        self.keychain_profile = keychain_profile or os.getenv("KEYCHAIN_PROFILE")
        self.zip_path = None

        if not self.developer_identity or not self.keychain_profile:
            raise RuntimeError("developer identity or keychain_profile not found")

    def print_colored(self, text: str, color: str):
        """Print text with colors using natural color names."""
        color_codes = {
            "black": "0;30",
            "red": "0;31",
            "green": "0;32",
            "yellow": "0;33",
            "blue": "0;34",
            "magenta": "0;35",
            "cyan": "0;36",
            "white": "0;37",
            "bright_black": "1;30",
            "bright_red": "1;31",
            "bright_green": "1;32",
            "bright_yellow": "1;33",
            "bright_blue": "1;34",
            "bright_magenta": "1;35",
            "bright_cyan": "1;36",
            "bright_white": "1;37",
        }
        color_code = color_codes.get(color.lower(), "0")
        print(f"\033[{color_code}m{text}\033[0m")

    def run_command(self, cmd: str, check: bool = True, capture_output: bool = False):
        """Run a shell command and return the result."""
        try:
            result = subprocess.run(
                cmd, shell=True, check=check, capture_output=capture_output, text=True
            )
            return result
        except subprocess.CalledProcessError as e:
            if check:
                self.print_colored(f"Command failed: {cmd}", "bright_red")
                sys.exit(1)
            return e

    def codesign(self):
        """Codesign the bundle using the provided developer identity."""
        self.print_colored(f"Codesigning {self.bundle_path}... ", "bright_green")

        cmd = f'codesign --force --options runtime --deep --sign "{self.dev_id}" "{self.bundle_path}"'
        result = self.run_command(cmd, check=False)

        if result.returncode == 0:
            self.print_colored("Codesign successful", "bright_green")
        else:
            self.print_colored("FAILED", "bright_red")
            sys.exit(1)

    def zip(self):
        """Zip the bundle using ditto."""
        self.print_colored(f"Zipping {self.bundle_path}... ", "bright_green")

        zip_path = f"{self.bundle_path}.zip"
        cmd = (
            f'/usr/bin/ditto -c -k --keepParent "{self.bundle_path}" "{self.zip_path}"'
        )
        result = self.run_command(cmd, check=False)

        if result.returncode == 0:
            self.print_colored("done", "bright_green")
            self.zip_path = Path(zip_path)
        else:
            self.print_colored("FAILED", "bright_red")
            sys.exit(1)

    def notarize(self):
        """Submit the app for notarization."""
        self.print_colored(
            "Sending package to Apple for notarization... ", "bright_green"
        )

        cmd = f'xcrun notarytool submit "{self.zip_path}" --keychain-profile "{self.keychain_profile}" --wait'
        self.run_command(cmd)

    def staple(self):
        """Staple the notarization ticket to the app."""
        self.print_colored(f"Stapling {self.bundle_path}... ", "bright_green")

        cmd = f'xcrun stapler staple "{self.bundle_path}"'
        result = self.run_command(cmd, check=False)

        if result.returncode == 0:
            self.print_colored("Success", "bright_green")
        else:
            self.print_colored(
                "FAILED, to see log use: xcrun notarytool log <Submission ID> --keychain-profile 'keychain_profile'",
                "bright_red",
            )
            sys.exit(1)

    def process(self):
        """main codesign and notarizing process"""

        # codesign bundle
        self.codesign()

        # zip bundle
        self.zip()

        # notarize bundle
        self.notarize()

        # Remove zip file
        self.zip_path.unlink()

        # staple bundle
        self.staple()

    @classmethod
    def commandline(cls):
        """commandline api"""

        parser = argparse.ArgumentParser(
            description="Sign, zip, notarize and staple a macOS bundle. Uses environmental variables when available.",
            formatter_class=argparse.RawDescriptionHelpFormatter,
            # epilog=cls.EPILOG,
        )

        parser.add_argument(
            "-d",
            "--developer-identity",
            help="Developer Identity",
            metavar="DEV_ID",
        )

        parser.add_argument(
            "-k",
            "--keychain-profile",
            help="Name of Keychain-stored Profile",
            metavar="KEYCHAIN_PROFILE",
        )

        parser.add_argument(
            "-m",
            "--manual",
            help="Display manual",
            action="store_true",
        )

        parser.add_argument("bundle_path", help="Path to the macOS app to process")

        args = parser.parse_args()

        if args.manual:
            print(cls.__doc__)

        app = cls(args.bundle_path, args.developer_identity, args.keychain_profile)

        app.process()


if __name__ == "__main__":
    BundleNotarizer.commandline()

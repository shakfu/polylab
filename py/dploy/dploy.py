#!/usr/bin/env python3
"""dploy.py

An attempt at creating a python clone of GNU stow that will work on Windows
as well as *nix.

This is a single-script variant of https://github.com/arecarn/dploy

"""

import abc
import argparse
import os
import pathlib
import shutil
import sys
from pathlib import Path
from collections import Counter, defaultdict

__version__ = "0.1.0"


# ----------------------------------------------------------------------------------
# errors


ERROR_HEAD = "dploy {subcmd}: can not {subcmd} "


class Errors:
    """A class that collects and executes action objects."""

    def __init__(self, is_silent: bool):
        self.exceptions: list[Exception] = []
        self.is_silent = is_silent

    def add(self, error: Exception):
        """Adds an error."""
        self.exceptions.append(error)

    def handle(self):
        """Prints and handles errors."""
        if len(self.exceptions) > 0:
            if not self.is_silent:
                for exception in self.exceptions:
                    print(exception, file=sys.stderr)
            raise self.exceptions[0]


class DployError(Exception):
    pass


class SourceIsSameAsDest(DployError):
    def __init__(self, subcmd, file):
        self.msg = (
            ERROR_HEAD + "'{file}': A source argument is the same as the dest argument"
        )
        self.msg = self.msg.format(subcmd=subcmd, file=file)
        # self.exception = ValueError(self.msg)

    def __str__(self):
        return self.msg


class ConflictsWithAnotherSource(DployError):
    def __init__(self, subcmd, files):
        self.msg = ERROR_HEAD + "the following: Conflicts with other source {files}"
        files_list = "\n    " + "\n    ".join(files)
        self.msg = self.msg.format(subcmd=subcmd, files=files_list)
        # self.exception = ValueError(self.msg)

    def __str__(self):
        return self.msg


class ConflictsWithExistingFile(DployError):
    def __init__(self, subcmd, source, dest):
        self.msg = ERROR_HEAD + "'{source}': Conflicts with existing file '{dest}'"
        self.msg = self.msg.format(subcmd=subcmd, source=source, dest=dest)
        # self.exception = ValueError(self.msg)

    def __str__(self):
        return self.msg


class ConflictsWithExistingLink(DployError):
    def __init__(self, subcmd, source, dest):
        self.msg = ERROR_HEAD + "'{source}': Conflicts with existing symlink '{dest}'"
        self.msg = self.msg.format(subcmd=subcmd, source=source, dest=dest)
        # self.exception = ValueError(self.msg)

    def __str__(self):
        return self.msg


class InsufficientPermissions(DployError):
    def __init__(self, subcmd, file):
        self.msg = ERROR_HEAD + "'{file}': Insufficient permissions"
        self.msg = self.msg.format(subcmd=subcmd, file=file)
        # self.exception = PermissionError(self.msg)

    def __str__(self):
        return self.msg


class NoSuchDirectory(DployError):
    def __init__(self, subcmd, file):
        self.msg = ERROR_HEAD + "'{file}': No such directory"
        self.msg = self.msg.format(subcmd=subcmd, file=file)
        # self.exception = NotADirectoryError(self.msg)

    def __str__(self):
        return self.msg


class PermissionDenied(DployError):
    def __init__(self, subcmd, file):
        self.msg = ERROR_HEAD + "'{file}': Permission denied"
        self.msg = self.msg.format(subcmd=subcmd, file=file)
        # self.exception = PermissionError(self.msg)

    def __str__(self):
        return self.msg


class InsufficientPermissionsToSubcmdFrom(DployError):
    def __init__(self, subcmd, file):
        self.msg = ERROR_HEAD + "from '{file}': Insufficient permissions"
        self.msg = self.msg.format(subcmd=subcmd, file=file)
        # self.exception = PermissionError(self.msg)

    def __str__(self):
        return self.msg


class NoSuchDirectoryToSubcmdInto(DployError):
    def __init__(self, subcmd, file):
        self.msg = ERROR_HEAD + "into '{file}': No such directory"
        self.msg = self.msg.format(subcmd=subcmd, file=file)
        # self.exception = NotADirectoryError(self.msg)

    def __str__(self):
        return self.msg


class InsufficientPermissionsToSubcmdTo(DployError):
    def __init__(self, subcmd, file):
        self.msg = ERROR_HEAD + "to '{file}': Insufficient permissions"
        self.msg = self.msg.format(subcmd=subcmd, file=file)
        # self.exception = PermissionError(self.msg)

    def __str__(self):
        return self.msg


class NoSuchFileOrDirectory(DployError):
    def __init__(self, subcmd, file):
        self.msg = ERROR_HEAD + "'{file}': No such file or directory"
        self.msg = self.msg.format(subcmd=subcmd, file=file)
        # self.exception = FileNotFoundError(self.msg)

    def __str__(self):
        return self.msg


class DuplicateSource(DployError):
    def __init__(self, subcmd, file):
        self.msg = ERROR_HEAD + "'{file}': Duplicate source argument"
        self.msg = self.msg.format(subcmd=subcmd, file=file)
        # self.exception = ValueError(self.msg)

    def __str__(self):
        return self.msg


# ----------------------------------------------------------------------------------
# utils


def get_directory_contents(directory: Path):
    """Return a sorted list of the contents of a directory."""
    contents = []

    for child in directory.iterdir():
        contents.append(child)

    return sorted(contents)


def rmtree(tree: Path):
    """Wrapper around shutil.rmtree to recursively delete a directory."""
    shutil.rmtree(tree)


def is_same_file(file1: Path, file2: Path):
    """Test if two pathlib.Path() objects are the same file.

    NOTE: python 3.5 supports pathlib.Path.samefile(file)
    NOTE: this can raise exception FileNotFoundError
    """
    return file1.resolve() == file2.resolve()


def is_same_files(files1: list[Path], files2: list[Path]):
    """Test if two collection of files are equivalent."""
    files1_resolved = [f.resolve() for f in files1]
    files2_resolved = [f.resolve() for f in files2]
    return files1_resolved == files2_resolved


def get_absolute_path(file: Path):
    """Get the absolute path of a pathlib.Path() object."""
    return file.expanduser().absolute()

def get_relative_path(path, start_at):
    """Get the relative path of a pathlib.Path() object.

    NOTE: python 3.4.5 & 3.5.2 support pathlib.Path.path =
    str(pathlib.Path)
    """
    try:
        relative_path = os.path.relpath(str(path), str(start_at))
    except ValueError:  # when a relative path does not exist
        return get_absolute_path(path)

    return pathlib.Path(relative_path)


def is_file_readable(a_file: Path):
    """Check if a pathlib.Path() file is readable."""
    return os.access(str(a_file), os.R_OK)


def is_file_writable(a_file: Path):
    """Check if a pathlib.Path() file is writable."""
    return os.access(str(a_file), os.W_OK)


def is_directory_readable(directory: Path):
    """Check if a pathlib.Path() directory is readable."""
    return os.access(str(directory), os.R_OK)


def is_directory_writable(directory: Path):
    """Check if a pathlib.Path() directory is writable."""
    return os.access(str(directory), os.W_OK)


def is_directory_executable(directory: Path):
    """Check if a pathlib.Path() directory is executable."""
    return os.access(str(directory), os.X_OK)


def readlink(path: Path, absolute_target: bool = False):
    """Get the target of a symbolic link passed as a pathlib.Path object and.

    provide the option to return an absolute path even if the link target is
    relative.

    Note: we can't use pathlib.Path.resolve because it doesn't work for broken
    links and raises FileNotFoundError in (Python <3.5) and always returns a
    relative path
    """
    link_target = os.readlink(str(path))
    path_dir = os.path.dirname(str(path))
    if absolute_target:
        if not os.path.isabs(link_target):
            link_target = os.path.join(path_dir, link_target)
        return pathlib.Path(link_target)
    return pathlib.Path(link_target)


# ----------------------------------------------------------------------------------
# actions


class Action(abc.ABC):
    """An abstract base class that define the interface for actions."""

    def __init__(self):
        pass

    @abc.abstractmethod
    def execute(self):
        """Function that executes the logic of each concrete action."""
        pass


class SymbolicLink(Action):
    """Action to create a symbolic link relative to the source of the link."""

    def __init__(self, subcmd, source, dest):
        super().__init__()
        self.source = source
        self.source_relative = get_relative_path(source, dest.parent)
        self.subcmd = subcmd
        self.dest = dest

    def execute(self):
        self.dest.symlink_to(self.source_relative)

    def __repr__(self):
        return "dploy {subcmd}: link {dest} => {source}".format(
            subcmd=self.subcmd, dest=self.dest, source=self.source_relative
        )


class AlreadyLinked(Action):
    """Action to used to print an already linked message."""

    def __init__(self, subcmd, source, dest):
        super().__init__()
        self.source = source
        self.source_relative = get_relative_path(source, dest.parent)
        self.dest = dest
        self.subcmd = subcmd

    def execute(self):
        pass

    def __repr__(self):
        return "dploy {subcmd}: already linked {dest} => {source}".format(
            subcmd=self.subcmd, source=self.source_relative, dest=self.dest
        )


class AlreadyUnlinked(Action):
    """Action to used to print an already unlinked message."""

    def __init__(self, subcmd, source, dest):
        super().__init__()
        self.source = source
        self.source_relative = get_relative_path(source, dest.parent)
        self.dest = dest
        self.subcmd = subcmd

    def execute(self):
        pass

    def __repr__(self):
        return "dploy {subcmd}: already unlinked {dest} => {source}".format(
            subcmd=self.subcmd, source=self.source_relative, dest=self.dest
        )


class UnLink(Action):
    """Action to unlink a symbolic link."""

    def __init__(self, subcmd, target):
        super().__init__()
        self.target = target
        self.subcmd = subcmd

    def execute(self):
        if not self.target.is_symlink():
            raise RuntimeError(
                "dploy detected and aborted an attempt to unlink a non-symlink {target} this is a bug and should be reported".format(
                    target=self.target
                )
            )
        self.target.unlink()

    def __repr__(self):
        return "dploy {subcmd}: unlink {target} => {source}".format(
            subcmd=self.subcmd, target=self.target, source=readlink(self.target)
        )


class MakeDirectory(Action):
    """Action to create a directory."""

    def __init__(self, subcmd, target):
        super().__init__()
        self.target = target
        self.subcmd = subcmd

    def execute(self):
        self.target.mkdir()

    def __repr__(self):
        return "dploy {subcmd}: make directory {target}".format(
            target=self.target, subcmd=self.subcmd
        )


class RemoveDirectory(Action):
    """Action to remove a directory."""

    def __init__(self, subcmd, target):
        super().__init__()
        self.target = target
        self.subcmd = subcmd

    def execute(self):
        self.target.rmdir()

    def __repr__(self):
        msg = "dploy {subcmd}: remove directory {target}"
        return msg.format(target=self.target, subcmd=self.subcmd)



class Actions:
    """A class that collects and executes action objects."""

    def __init__(self, is_silent: bool, is_dry_run: bool):
        self.actions: list[Action] = []
        self.is_silent = is_silent
        self.is_dry_run = is_dry_run

    def add(self, action: Action):
        """Adds an action."""
        self.actions.append(action)

    def execute(self):
        """Prints and executes actions."""
        for action in self.actions:
            if not self.is_silent:
                print(action)
            if not self.is_dry_run:
                action.execute()

    def get_unlink_actions(self):
        """Get the current Unlink() actions from the self.actions."""
        return [a for a in self.actions if isinstance(a, UnLink)]

    def get_unlink_target_parents(self):
        """Get list of the parents for the current Unlink() actions from.

        self.actions
        """
        unlink_actions = self.get_unlink_actions()
        # sort for deterministic output
        return sorted(set([a.target.parent for a in unlink_actions]))

    def get_unlink_targets(self):
        """Get list of the targets for the current Unlink() actions from.

        self.actions
        """
        unlink_actions = self.get_unlink_actions()
        return [a.target for a in unlink_actions]

    def get_duplicates(self):
        """Return a tuple containing tuples with the following structure.

        (link destination, [indices of duplicates])
        """
        tally = defaultdict(list)
        for index, action in enumerate(self.actions):
            if isinstance(action, SymbolicLink):
                tally[action.dest].append(index)
        # sort for deterministic output
        return sorted([indices for _, indices in tally.items() if len(indices) > 1])

# ----------------------------------------------------------------------------------
# ignore


class Ignore:
    """Handles ignoring of files via glob patterns either passed in directly in or.

    in a specified ignore file.
    """

    def __init__(self, patterns, source):
        if patterns is None:
            input_patterns = []
        else:
            input_patterns = patterns
        self.ignored_files = []

        file = source.parent / pathlib.Path(".dploystowignore")

        self.patterns = [str(file.name)]  # ignore the ignore file
        self.patterns.extend(input_patterns)
        self._read_ignore_file_patterns(file)

    def _read_ignore_file_patterns(self, file):
        """Read ignore patterns from a specified file."""
        try:
            with open(str(file)) as afile:
                file_patterns = afile.read().splitlines()
                self.patterns.extend(file_patterns)
        except FileNotFoundError:
            pass

    def should_ignore(self, source):
        """Check if a source should be ignored, based on the ignore patterns in.

        self.patterns

        This checks if the ignore patterns match either the file exactly or
        its parents
        """
        for pattern in self.patterns:
            try:
                files = sorted(source.parent.glob(pattern))
            except IndexError:  # the glob result was empty
                continue

            for file in files:
                if is_same_file(file, source) or source in file.parents:
                    return True
        return False

    def ignore(self, file):
        """Add a file to be ignored."""
        self.ignored_files.append(file)

    def get_ignored_files(self):
        """Get a list of the files that have been ignored."""
        return self.ignored_files


# -----------------------------------------------------------------------------
# main


class Input:
    """Input validator abstract base class."""

    def __init__(self, errors, subcmd):
        self.errors = errors
        self.subcmd = subcmd

    def is_valid(self, sources, dest):
        """Checks if the passes in source and dest are valid."""
        is_input_valid = True
        if not self._is_there_duplicate_sources(sources) and self._is_valid_dest(dest):
            for source in sources:
                if not self._is_valid_source(source):
                    is_input_valid = False
        else:
            is_input_valid = False

        return is_input_valid

    def _is_there_duplicate_sources(self, sources):
        """Checks sources to see if there are any duplicates."""

        is_there_duplicates = False

        tally = defaultdict(int)
        for source in sources:
            tally[source] += 1

        for source, count in tally.items():
            if count > 1:
                is_there_duplicates = True
                self.errors.add(DuplicateSource(self.subcmd, source))

        return is_there_duplicates

    def _is_valid_dest(self, dest):
        """Abstract method to check if the dest input to a sub-command is valid."""
        pass

    def _is_valid_source(self, source):
        """Abstract method to check if the source input to a sub-command is valid."""
        pass


class AbstractBaseSubCommand:
    """An abstract class to unify shared functionality in stow commands."""

    def __init__(self, subcmd, sources, dest, is_silent: bool, is_dry_run: bool, ignore_patterns):
        self.subcmd = subcmd

        self.actions = Actions(is_silent, is_dry_run)
        self.errors = Errors(is_silent)

        self.is_silent = is_silent
        self.is_dry_run = is_dry_run

        self.dest_input = pathlib.Path(dest)
        source_inputs = [pathlib.Path(source) for source in sources]

        if self._is_valid_input(source_inputs, self.dest_input):
            for source in source_inputs:
                self.ignore = Ignore(ignore_patterns, source)

                if self.ignore.should_ignore(source):
                    self.ignore.ignore(source)
                    continue

                self._collect_actions(source, self.dest_input)

        self._check_for_other_actions()
        self._execute_actions()

    def _check_for_other_actions(self):
        """Abstract method for examine the existing action to see if more actions.

        need to be added or if some actions need to be removed.
        """
        pass

    def _is_valid_input(self, sources, dest):
        """Abstract method to check if the input to a sub-command is valid."""
        pass

    def _collect_actions(self, source, dest):
        """Abstract method that collects the actions required to complete a.

        sub-command.
        """
        pass

    def _execute_actions(self):
        """Either executes collected actions by a sub command or raises collected.

        exceptions.
        """
        self.errors.handle()
        self.actions.execute()


# -----------------------------------------------------------------------------
# stowcmd


class AbstractBaseStow(AbstractBaseSubCommand):
    """Abstract Base class containing shared logic for all stow commands."""

    def __init__(self, subcmd, source, dest, is_silent, is_dry_run, ignore_patterns):
        self.is_unfolding = False
        super().__init__(subcmd, source, dest, is_silent, is_dry_run, ignore_patterns)

    def _is_valid_input(self, sources, dest):
        """Check to see if the input is valid."""
        return StowInput(self.errors, self.subcmd).is_valid(sources, dest)

    def get_directory_contents(self, directory: Path):
        """Get the contents of a directory while handling errors that may occur."""
        contents = []

        try:
            contents = get_directory_contents(directory)
        except PermissionError:
            self.errors.add(PermissionDenied(self.subcmd, directory))
        except FileNotFoundError:
            self.errors.add(NoSuchFileOrDirectory(self.subcmd, directory))
        except NotADirectoryError:
            self.errors.add(NoSuchDirectory(self.subcmd, directory))

        return contents

    def _are_same_file(self, source, dest):
        """Abstract method that handles the case when the source and dest are the.

        same file when collecting actions
        """
        pass

    def _are_directories(self, source, dest):
        """Abstract method that handles the case when the source and dest are directories.

        same file when collecting actions
        """
        pass

    def _are_other(self, source, dest):
        """Abstract method that handles all other cases what to do if no particular.

        condition is true cases are found
        """
        pass

    def _collect_actions_existing_dest(self, source, dest):
        """_collect_actions() helper to collect required actions to perform a stow.

        command when the destination already exists
        """
        if is_same_file(dest, source):
            if dest.is_symlink() or self.is_unfolding:
                self._are_same_file(source, dest)
            else:
                self.errors.add(SourceIsSameAsDest(self.subcmd, dest.parent))

        elif dest.is_dir() and source.is_dir:
            self._are_directories(source, dest)
        else:
            self.errors.add(ConflictsWithExistingFile(self.subcmd, source, dest))

    def _collect_actions(self, source, dest):
        """Concrete method to collect required actions to perform a stow.

        sub-command
        """

        if self.ignore.should_ignore(source):
            self.ignore.ignore(source)
            return

        if not StowInput(self.errors, self.subcmd).is_valid_collection_input(
            source, dest
        ):
            return

        sources = self.get_directory_contents(source)

        for subsources in sources:
            if self.ignore.should_ignore(subsources):
                self.ignore.ignore(subsources)
                continue

            dest_path = dest / pathlib.Path(subsources.name)

            does_dest_path_exist = False
            try:
                does_dest_path_exist = dest_path.exists()
            except PermissionError:
                self.errors.add(PermissionDenied(self.subcmd, dest_path))
                return

            if does_dest_path_exist:
                self._collect_actions_existing_dest(subsources, dest_path)
            elif dest_path.is_symlink():
                self.errors.add(
                    ConflictsWithExistingLink(self.subcmd, subsources, dest_path)
                )
            elif not dest_path.parent.exists() and not self.is_unfolding:
                self.errors.add(NoSuchDirectory(self.subcmd, dest_path.parent))
            else:
                self._are_other(subsources, dest_path)


class Stow(AbstractBaseStow):
    """Concrete class implementation of the stow sub-command."""

    def __init__(
        self, source, dest, is_silent=True, is_dry_run=False, ignore_patterns=None
    ):
        super().__init__("stow", source, dest, is_silent, is_dry_run, ignore_patterns)

    def _unfold(self, source, dest):
        """Method unfold a destination directory."""
        self.is_unfolding = True
        self.actions.add(UnLink(self.subcmd, dest))
        self.actions.add(MakeDirectory(self.subcmd, dest))
        self._collect_actions(source, dest)
        self.is_unfolding = False

    def _handle_duplicate_actions(self):
        """Check for symbolic link actions that would cause conflicting symbolic.

        links to the same destination. Also check for actions that conflict but
        are candidates for unfolding instead.
        """
        has_conflicts = False
        dupes = self.actions.get_duplicates()

        if len(dupes) == 0:
            return

        for indices in dupes:
            first_action = self.actions.actions[indices[0]]
            remaining_actions = [self.actions.actions[i] for i in indices[1:]]

            if first_action.source.is_dir():
                self._unfold(first_action.source, first_action.dest)

                for action in remaining_actions:
                    self.is_unfolding = True
                    self._collect_actions(action.source, action.dest)
                    self.is_unfolding = False
            else:
                duplicate_action_sources = [
                    str(self.actions.actions[i].source) for i in indices
                ]
                self.errors.add(
                    ConflictsWithAnotherSource(self.subcmd, duplicate_action_sources)
                )
                has_conflicts = True

        if has_conflicts:
            return

        # remove duplicates
        for indices in dupes:
            for index in reversed(indices[1:]):
                del self.actions.actions[index]

        self._handle_duplicate_actions()

    def _check_for_other_actions(self):
        self._handle_duplicate_actions()

    def _are_same_file(self, source, dest):
        """What to do if source and dest are the same files."""
        if self.is_unfolding:
            self.actions.add(SymbolicLink(self.subcmd, source, dest))
        else:
            self.actions.add(AlreadyLinked(self.subcmd, source, dest))

    def _are_directories(self, source, dest):
        if dest.is_symlink():
            self._unfold(dest.resolve(), dest)
        self._collect_actions(source, dest)

    def _are_other(self, source, dest):
        self.actions.add(SymbolicLink(self.subcmd, source, dest))


class UnStow(AbstractBaseStow):
    """Concrete class implementation of the unstow sub-command."""

    def __init__(
        self, source, dest, is_silent=True, is_dry_run=False, ignore_patterns=None
    ):
        super().__init__("unstow", source, dest, is_silent, is_dry_run, ignore_patterns)

    def _are_same_file(self, source, dest):
        """What to do if source and dest are the same files."""
        self.actions.add(UnLink(self.subcmd, dest))

    def _are_directories(self, source, dest):
        self._collect_actions(source, dest)

    def _are_other(self, source, dest):
        self.actions.add(AlreadyUnlinked(self.subcmd, source, dest))

    def _check_for_other_actions(self):
        self._collect_folding_actions()

    def _collect_folding_actions(self):
        """
        Find candidates for folding i.e. when a directory contains symlinks to.

        files that all share the same parent directory
        """
        for parent in self.actions.get_unlink_target_parents():
            items = get_directory_contents(parent)
            other_links_parents = []
            other_links = []
            source_parent = None
            is_normal_files_detected = False

            for item in items:
                if item not in self.actions.get_unlink_targets():
                    does_item_exist = False
                    try:
                        does_item_exist = item.exists()
                    except PermissionError:
                        self.errors.add(PermissionDenied(self.subcmd, item))
                        return

                    if does_item_exist and item.is_symlink():
                        source_parent = item.resolve().parent
                        other_links_parents.append(item.resolve().parent)
                        other_links.append(item)
                    else:
                        is_normal_files_detected = True
                        break

            if not is_normal_files_detected:
                other_links_parent_count = len(Counter(other_links_parents))

                if other_links_parent_count == 1:
                    assert source_parent is not None
                    if is_same_files(
                        get_directory_contents(source_parent), other_links
                    ):
                        self._fold(source_parent, parent)

                elif other_links_parent_count == 0 and not is_same_file(
                    parent, self.dest_input
                ):
                    self.actions.add(RemoveDirectory(self.subcmd, parent))

    def _fold(self, source, dest):
        """Add the required actions for folding."""
        self._collect_actions(source, dest)
        self.actions.add(RemoveDirectory(self.subcmd, dest))
        self.actions.add(SymbolicLink(self.subcmd, source, dest))


class StowInput(Input):
    """Input validator for the link command."""

    def _is_valid_dest(self, dest):
        """Check if the test argument is valid."""
        result = True

        if not dest.is_dir():
            self.errors.add(NoSuchDirectoryToSubcmdInto(self.subcmd, dest))
            result = False
        else:
            if not is_directory_writable(dest):
                self.errors.add(InsufficientPermissionsToSubcmdTo(self.subcmd, dest))
                result = False

            if not is_directory_readable(dest):
                self.errors.add(InsufficientPermissionsToSubcmdTo(self.subcmd, dest))
                result = False

            if not is_directory_executable(dest):
                self.errors.add(InsufficientPermissionsToSubcmdTo(self.subcmd, dest))
                result = False

        return result

    def _is_valid_source(self, source):
        """Check if the source argument is valid."""
        result = True

        if not source.is_dir():
            self.errors.add(NoSuchDirectory(self.subcmd, source))
            result = False
        else:
            if not is_directory_readable(source):
                self.errors.add(
                    InsufficientPermissionsToSubcmdFrom(self.subcmd, source)
                )
                result = False

            if not is_directory_executable(source):
                self.errors.add(
                    InsufficientPermissionsToSubcmdFrom(self.subcmd, source)
                )
                result = False

        return result

    def is_valid_collection_input(self, source, dest):
        """Helper to validate the source and dest parameters passed to.

        _collect_actions()
        """
        result = True
        if not self._is_valid_source(source):
            result = False

        if dest.exists():
            if not self._is_valid_dest(dest):
                result = False
        return result


class Clean(AbstractBaseSubCommand):
    """Abstract Base class that contains the shared logic for all of the stow.

    commands
    """

    def __init__(self, source, dest, is_silent, is_dry_run, ignore_patterns):
        self.source = [pathlib.Path(s) for s in source]
        self.dest = pathlib.Path(dest)
        self.ignore_patterns = ignore_patterns
        super().__init__("clean", source, dest, is_silent, is_dry_run, ignore_patterns)

    def _is_valid_input(self, sources, dest):
        """Check to see if the input is valid."""
        return StowInput(self.errors, self.subcmd).is_valid(sources, dest)

    def get_directory_contents(self, directory):
        """Get the contents of a directory while handling errors that may occur."""
        contents = []

        try:
            contents = get_directory_contents(directory)
        except PermissionError:
            self.errors.add(PermissionDenied(self.subcmd, directory))
        except FileNotFoundError:
            self.errors.add(NoSuchFileOrDirectory(self.subcmd, directory))
        except NotADirectoryError:
            self.errors.add(NoSuchDirectory(self.subcmd, directory))

        return contents

    def _collect_clean_actions(self, source, source_names, dest):
        subdests = get_directory_contents(dest)
        for subdest in subdests:
            if subdest.is_symlink():
                link_target = readlink(subdest, absolute_target=True)
                if not link_target.exists() and not source_names.isdisjoint(
                    set(link_target.parents)
                ):
                    self.actions.add(UnLink(self.subcmd, subdest))
            elif subdest.is_dir():
                self._collect_clean_actions(source, source_names, subdest)

    def _check_for_other_actions(self):
        """Concrete method to collect required actions to perform a stow.

        sub-command
        """
        valid_files = []
        for a_file in self.source:
            self.ignore = Ignore(self.ignore_patterns, a_file)
            if self.ignore.should_ignore(a_file):
                self.ignore.ignore(a_file)
                continue
            else:
                valid_files.append(a_file)

            if not StowInput(self.errors, self.subcmd).is_valid_collection_input(
                a_file, self.dest
            ):
                return

        # NOTE: an option to make clean more aggressive is to change f.name to
        # f.parent this could a be a good --option
        files_names = [get_absolute_path(f.name) for f in valid_files]
        files_names_set = set(files_names)
        self._collect_clean_actions(valid_files, files_names_set, self.dest)


# -----------------------------------------------------------------------------
# linkcmd


class Link(AbstractBaseSubCommand):
    """Concrete class implementation of the link sub-command."""

    def __init__(
        self, source, dest, is_silent=True, is_dry_run=False, ignore_patterns=None
    ):
        super().__init__("link", [source], dest, is_silent, is_dry_run, ignore_patterns)

    def _is_valid_input(self, sources, dest):
        """Check to see if the input is valid."""
        return LinkInput(self.errors, self.subcmd).is_valid(sources, dest)

    def _collect_actions(self, source, dest):
        """Concrete method to collect required actions to perform a link.

        sub-command
        """

        if dest.exists():
            if is_same_file(dest, source):
                self.actions.add(AlreadyLinked(self.subcmd, source, dest))
            else:
                self.errors.add(ConflictsWithExistingFile(self.subcmd, source, dest))
        elif dest.is_symlink():
            self.errors.add(ConflictsWithExistingLink(self.subcmd, source, dest))

        elif not dest.parent.exists():
            self.errors.add(NoSuchDirectoryToSubcmdInto(self.subcmd, dest.parent))

        else:
            self.actions.add(SymbolicLink(self.subcmd, source, dest))


class LinkInput(Input):
    """Input validator for the link command."""

    def _is_valid_dest(self, dest):
        if not dest.parent.exists():
            self.errors.add(NoSuchFileOrDirectory(self.subcmd, dest.parent))
            return False

        elif not is_file_writable(dest.parent) or not is_directory_writable(
            dest.parent
        ):
            self.errors.add(InsufficientPermissionsToSubcmdTo(self.subcmd, dest))
            return False

        return True

    def _is_valid_source(self, source):
        if not source.exists():
            self.errors.add(NoSuchFileOrDirectory(self.subcmd, source))
            return False

        elif not is_file_readable(source) or not is_directory_readable(source):
            self.errors.add(InsufficientPermissions(self.subcmd, source))
            return False

        return True


# ----------------------------------------------------------------------------------
# __init__


def stow(sources, dest, is_silent=True, is_dry_run=False, ignore_patterns=None):
    """Sub command stow."""
    Stow(sources, dest, is_silent, is_dry_run, ignore_patterns)


def unstow(sources, dest, is_silent=True, is_dry_run=False, ignore_patterns=None):
    """Sub command unstow."""
    UnStow(sources, dest, is_silent, is_dry_run, ignore_patterns)


def clean(sources, dest, is_silent=True, is_dry_run=False, ignore_patterns=None):
    """Sub command clean."""
    Clean(sources, dest, is_silent, is_dry_run, ignore_patterns)


def link(source, dest, is_silent=True, is_dry_run=False, ignore_patterns=None):
    """Sub command link."""
    Link(source, dest, is_silent, is_dry_run, ignore_patterns)


# ----------------------------------------------------------------------------------
# __main__


def add_ignore_argument(parser):
    """Adds the ignore argument to a subcmd parser."""
    parser.add_argument(
        "--ignore",
        dest="ignore_patterns",
        action="append",
        default=None,
        help="glob pattern used to ignore directories",
    )


def create_parser():
    """Create the CLI argument parser."""
    parser = argparse.ArgumentParser(prog="dploy")

    parser.add_argument(
        "--version",
        action="version",
        version="%(prog)s {version}".format(version=__version__),
    )
    parser.add_argument(
        "--silent",
        dest="is_silent",
        action="store_true",
        help="suppress all output",
    )
    parser.add_argument(
        "--dry-run",
        dest="is_dry_run",
        action="store_true",
        help="show what would be done without doing it",
    )

    sub_parsers = parser.add_subparsers(dest="subcmd")

    stow_parser = sub_parsers.add_parser("stow")
    stow_parser.add_argument("source", nargs="+", help="source directory to stow")
    stow_parser.add_argument("dest", help="destination path to stow into")
    add_ignore_argument(stow_parser)

    unstow_parser = sub_parsers.add_parser("unstow")
    unstow_parser.add_argument(
        "source", nargs="+", help="source directory to unstow from"
    )
    unstow_parser.add_argument("dest", help="destination path to unstow")
    add_ignore_argument(unstow_parser)

    clean_parser = sub_parsers.add_parser("clean")
    clean_parser.add_argument(
        "source", nargs="+", help="source directory to clean from"
    )
    clean_parser.add_argument("dest", help="destination path to clean")
    add_ignore_argument(clean_parser)

    link_parser = sub_parsers.add_parser("link")
    link_parser.add_argument("source", help="source file or directory to link")
    link_parser.add_argument("dest", help="destination path to link")
    add_ignore_argument(link_parser)
    return parser


def run(arguments=None):
    """Interpret the parser arguments and execute the corresponding commands."""

    subcmd_map = {
        "stow": Stow,
        "unstow": UnStow,
        "clean": Clean,
        "link": Link,
    }

    try:
        parser = create_parser()

        if arguments is None:
            args = parser.parse_args()
        else:
            args = parser.parse_args(arguments)

        if args.subcmd in subcmd_map:
            subcmd = subcmd_map[args.subcmd]
        else:
            parser.print_help()
            sys.exit(0)

        try:
            subcmd(
                args.source,
                args.dest,
                is_silent=args.is_silent,
                is_dry_run=args.is_dry_run,
                ignore_patterns=args.ignore_patterns,
            )
        except DployError:
            sys.exit(1)

    except KeyboardInterrupt as error:
        print(error, file=sys.stderr)
        sys.exit(130)


if __name__ == "__main__":
    run()

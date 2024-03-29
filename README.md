# Systranything

[![CI][status-png]][status]

`systranything` lets you put anything in your system tray. It uses a YAML file 
which describes the icon to put in the system tray along a context menu and 
callbacks to be executed in a shell.

The menu can contain labels, separators, submenus, checkboxes and radiobuttons. 
Scroll events can be triggered on the main icon.

Things you can do with it:

- a custom launcher menu
- a volume icon
- a menu to turn on or off your VPN
- a menu to toggle dual monitor setups
- anything that requires a status icon and actions

See [the example file](example.yaml) to get started.

Run it with:

```bash
$ systranything -f ./example.yaml
```

It has a verbose mode which can be turned on with `-v`. It writes on `stdout` 
the commands executed along their outputs.

# Hacking

The project can be built with [nix][nix].

Install with:

```bash
$ nix profile install
```

Build with:

```bash
$ nix build
```

The binary is then created in `./result/bin/systranything`

Hack with:

```bash
$ nix develop
```

You will be dropped in a shell with all the needed tools in scope: `cabal` to 
build the project and `haskell-language-server` for a better developer 
experience.

[nix]: https://nixos.org/
[status-png]: https://github.com/jecaro/systranything/workflows/CI/badge.svg
[status]: https://github.com/jecaro/systranything/actions

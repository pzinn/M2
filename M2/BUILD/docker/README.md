# Using Macaulay2 with Docker

[Docker](https://docs.docker.com/get-docker/) is a containerization program for building, shipping, or running applications on different platforms.

The `Dockerfile` in this directory creates a container image based on latest Ubuntu distribution with Macaulay2 installed from a [PPA repository](https://launchpad.net/~macaulay2/+archive/ubuntu/macaulay2). Each subdirectory contains a `Dockerfile` designed for other use cases:

- [`ubuntu`](ubuntu):   Compiling Macaulay2 in a Container
- [`debian`](debian):   Packaging Macaulay2 for Debian (.deb)
- [`fedora`](fedora):   Packaging Macaulay2 for Fedora (.rpm)
- [`rhel`](rhel):       Packaging Macaulay2 for Red Hat Enterprise Linux (.rpm)
- [`testbot`](testbot): Macaulay2 testbot for GitHub Actions
- [`brew`](brew):       Bottling  Macaulay2 for [Homebrew](https://brew.sh/)
- [`nightly`](nightly): Testing the Nightly Build of Macaulay2
- [`actions`](actions): Testing the GitHub Actions Build

**Note**: some of the above may be outdated and require slight modifications before use.

## Getting Started
0. Install Docker and start the daemon.

1. Build a Macaulay2 container:
```
make build
```

2. Run Macaulay2:
```
make run
```

This command is the same as `make run-emacs`, which opens Emacs with M2 running from the container. If the graphical interface is not working, run `make run-emacs-tui` instead. To run `M2` directly, you can use `make run-M2` or `make run-M2-safe`.

## Accessing files and packages

A local directory named `storage` is shared as the `/home/macaulay` directory within the container. Furthermore, the current Macaulay2 git repository is available at `/home/macaulay/M2`.

## Accessing a terminal

You can use `make shell` to start bash within the container or `make root` to start a root terminal.

*Note*: changes outside the home directory within the container are ephemeral. To modify the container, edit the `Dockerfile` and run `make build` again.

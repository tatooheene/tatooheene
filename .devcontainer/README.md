# DevContainer Setup

This directory contains the development container configuration for the tatooheene R package.

## Quick Start

1. Install [Docker Desktop](https://www.docker.com/products/docker-desktop)
2. Install VS Code with the [Dev Containers extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers)
3. Open this repository in VS Code
4. Click "Reopen in Container" when prompted

## Files

- **`devcontainer.json`** - Main configuration file
  - Defines the development environment
  - Configures VS Code settings and extensions
  - Sets up SSH agent forwarding for git

- **`Dockerfile`** - Container image definition
  - Based on `rocker/r-ver:4.4`
  - Pre-installs all R development packages
  - Configures system dependencies

- **`setup.sh`** - Post-creation setup script
  - Installs project-specific package dependencies
  - Generates documentation
  - Runs on container creation

## Pre-built Image

For faster startup, a GitHub Action automatically builds and publishes the container image.

### Using the Pre-built Image

After the first GitHub Action run:

1. Check that your image exists at: `https://github.com/OWNER/tatooheene/pkgs/container/tatooheene-devcontainer`

2. Edit `devcontainer.json`:
   ```json
   {
     "name": "tatooheene R Package Development",
     // Comment out build:
     // "build": { "dockerfile": "Dockerfile", "context": "." },

     // Use pre-built image (replace OWNER):
     "image": "ghcr.io/OWNER/tatooheene-devcontainer:latest",
     ...
   }
   ```

3. Rebuild container (Cmd/Ctrl+Shift+P → "Dev Containers: Rebuild Container")

### Image Build Times

- **Local build** (first time): ~10-15 minutes
- **Pre-built download**: ~2-3 minutes
- **Cached startup**: < 10 seconds

## Installed R Packages

Development packages pre-installed in the image:

- **Core**: devtools, roxygen2, testthat, usethis, remotes
- **Code Quality**: styler, lintr, covr
- **Documentation**: pkgdown
- **Language Server**: languageserver (for IDE features)

Project-specific dependencies are installed from `DESCRIPTION` during container creation.

## Updating the Image

When you modify the Dockerfile or package list:

1. **Local Development**:
   - Changes are automatically detected
   - Rebuild: Cmd/Ctrl+Shift+P → "Dev Containers: Rebuild Container"

2. **Pre-built Image**:
   - Push changes to `.devcontainer/**`
   - GitHub Action automatically rebuilds
   - Pull updated image on next container start

## Git Configuration

The devcontainer automatically configures git for you:

- **Git + SSH**: Pre-installed in the image (git, openssh-client)
- **User Config**: VSCode automatically copies `user.name` and `user.email` from your host machine
- **SSH Keys**: Forwarded from host via SSH agent (no keys in container)

### Verify Git is Configured

```bash
# Check git is installed
git --version

# Check user configuration
git config --list | grep user

# Should show:
# user.name=Your Name
# user.email=your@email.com
```

If `user.name` or `user.email` are missing, VSCode should set them automatically on next container start. You can also set them manually:

```bash
git config --global user.name "Your Name"
git config --global user.email "your@email.com"
```

## Troubleshooting

### Container won't start
```bash
# Clear Docker cache
Dev Containers: Clean / Purge Data

# Check Docker is running
docker ps
```

### R packages missing
```r
# Reinstall from DESCRIPTION
devtools::install_deps(dependencies = TRUE)

# Or install specific package
install.packages("package_name")
```
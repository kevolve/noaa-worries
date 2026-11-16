# =====================================================================
# Ensure the conda environment used by the CRW archive tools is active,
# creating it from environment.yml on first use.
#
# This file is SOURCED, not executed - activation has to happen in the
# calling shell. B_hpc_scripts/update_archive.slurm sources it
# automatically; source it by hand before running a command:
#
#   source A_scripts/environment/activate_env.sh
#   python3 A_scripts/functions/crw_archive.py check --network
#
# Environment variables:
#   CRW_CONDA_ENV       environment name        (default "crw")
#   CRW_REPO            repository root         (default: two levels above this file)
#   CRW_SKIP_CONDA      set to 1 to leave the current environment alone,
#                       for a manually managed setup
#   CRW_SKIP_PKG_CHECK  set to 1 to skip the package check
#
# Used only where an environment-module system exists, i.e. on the HPC:
#   CRW_CONDA_MODULE    conda module   (default "miniforge/25.3.0-3")
#   CRW_R_MODULE        R module       (default "r/4.4.0-gfbf-2023a")
#                       set either to an empty string to load none
#   CRW_SKIP_MODULE_PURGE  set to 1 to keep already-loaded modules
#                       (default: `module purge` runs first)
#
# R comes from the HPC module (or the system R on a local machine); the
# conda environment supplies Python only.
#
# (CRW_ROOT is separate: it sets the data root, not the repository root.)
#
# Exit status is 0 when the environment is ready and 1 otherwise, so a
# caller can write:  source A_scripts/environment/activate_env.sh || exit 1
# =====================================================================

if [ -n "${BASH_SOURCE:-}" ] && [ "${BASH_SOURCE[0]}" = "${0}" ]; then
  echo "activate_env.sh must be sourced, not executed:" >&2
  echo "  source A_scripts/environment/activate_env.sh" >&2
  exit 1
fi

# ---------------------------------------------------------------------
# Confirm the active environment holds what the tools need.
#
# An environment created from an older environment.yml is reused as it
# is, so a package added to environment.yml later would be silently
# missing. Missing Python packages are an error (the archive cannot be
# built); missing R packages only produce a note (reading and analysis).
# ---------------------------------------------------------------------
crw_check_packages() {
  local env_name missing_py missing_r conda_pkgs

  env_name="${CRW_CONDA_ENV:-crw}"

  if [ "${CRW_SKIP_PKG_CHECK:-0}" = "1" ]; then
    return 0
  fi

  if command -v python3 >/dev/null 2>&1; then
    missing_py="$(python3 -c 'import importlib.util as u; print(" ".join(m for m in ("numpy","netCDF4","pyarrow") if u.find_spec(m) is None))' 2>/dev/null)"
  else
    missing_py="python3"
  fi

  if command -v Rscript >/dev/null 2>&1; then
    missing_r="$(Rscript --no-init-file --no-site-file -e 'p <- c("arrow","dplyr","jsonlite","tibble","ggplot2"); cat(paste(p[!vapply(p, requireNamespace, logical(1), quietly = TRUE)], collapse = " "))' 2>/dev/null)"
  else
    missing_r="Rscript"
  fi

  if [ "$missing_r" = "Rscript" ]; then
    echo "NOTE: Rscript was not found; R is needed only for reading the archive." >&2
  elif [ -n "$missing_r" ]; then
    echo "NOTE: R packages for reading the archive are missing: $missing_r" >&2
    echo "  Rscript -e 'install.packages(c($(printf '"%s",' $missing_r | sed 's/,$//')), repos = \"https://cloud.r-project.org\")'" >&2
  fi

  if [ -z "$missing_py" ]; then
    return 0
  fi

  # conda-forge package names are lowercase (netCDF4 -> netcdf4).
  conda_pkgs="$(printf '%s' "$missing_py" | tr 'A-Z' 'a-z')"

  echo "ERROR: the active environment is missing Python packages needed to build the archive." >&2
  echo "  Python: $missing_py" >&2
  if [ "${CRW_SKIP_CONDA:-0}" = "1" ]; then
    echo "  Install them into the environment in use:" >&2
    echo "    python3 -m pip install -r A_scripts/functions/requirements.txt" >&2
    echo "  or unset CRW_SKIP_CONDA to use the managed conda environment." >&2
  else
    cat >&2 <<MSG

  Install just what is missing:

    conda install -n $env_name -c conda-forge $conda_pkgs

  or bring the whole environment up to environment.yml:

    conda env update -n $env_name -f environment.yml --prune

  If conda's libmamba solver errors, add --solver=classic.
  Set CRW_SKIP_PKG_CHECK=1 to bypass this check.
MSG
  fi
  return 1
}

# ---------------------------------------------------------------------
# On systems with environment modules (the HPC): purge loaded modules,
# then load the conda and R modules. Skipped entirely elsewhere.
# A missing conda module is fatal only if conda cannot be found another
# way (checked later); a missing R module is reported but not fatal,
# because building the archive needs Python only.
# ---------------------------------------------------------------------
crw_load_modules() {
  local conda_module r_module had_e had_u

  if ! command -v module >/dev/null 2>&1; then
    return 0
  fi
  conda_module="${CRW_CONDA_MODULE-miniforge/25.3.0-3}"
  r_module="${CRW_R_MODULE-r/4.4.0-gfbf-2023a}"

  # module is a shell function not written for `set -e` / `set -u`.
  case $- in *e*) had_e=1 ;; *) had_e=0 ;; esac
  case $- in *u*) had_u=1 ;; *) had_u=0 ;; esac
  set +eu

  if [ "${CRW_SKIP_MODULE_PURGE:-0}" != "1" ]; then
    module purge
  fi
  if [ -n "$conda_module" ]; then
    if module load "$conda_module"; then
      echo "Loaded module '$conda_module'."
    else
      echo "NOTE: could not load conda module '$conda_module'. Check" >&2
      echo "      'module avail miniforge' and set CRW_CONDA_MODULE." >&2
    fi
  fi
  if [ -n "$r_module" ]; then
    if module load "$r_module" && command -v Rscript >/dev/null 2>&1; then
      echo "Loaded module '$r_module'."
    else
      echo "NOTE: could not load R module '$r_module'. Check 'module avail r'" >&2
      echo "      and set CRW_R_MODULE to the right name." >&2
    fi
  fi

  if [ "$had_e" = 1 ]; then set -e; fi
  if [ "$had_u" = 1 ]; then set -u; fi
  return 0
}

crw_activate_env() {
  local env_name here root yml conda_exe solver shell_name had_e had_u rc candidate

  env_name="${CRW_CONDA_ENV:-crw}"

  if [ "${CRW_SKIP_CONDA:-0}" = "1" ]; then
    echo "CRW_SKIP_CONDA=1: using the current environment unchanged."
    return 0
  fi

  if [ "${CONDA_DEFAULT_ENV:-}" = "$env_name" ]; then
    echo "conda environment '$env_name' is already active."
    return 0
  fi

  # ---- locate the repository root -----------------------------------
  if [ -n "${CRW_REPO:-}" ]; then
    root="$CRW_REPO"
  elif [ -n "${BASH_SOURCE:-}" ]; then
    here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    root="$(dirname "$(dirname "$here")")"   # A_scripts/environment -> repository root
  else
    root="$PWD"
  fi
  yml="$root/environment.yml"

  # ---- locate conda -------------------------------------------------
  conda_exe=""
  if command -v conda >/dev/null 2>&1; then
    conda_exe="$(command -v conda)"
  else
    for candidate in \
      "${CONDA_EXE:-}" \
      "$HOME/miniforge3/bin/conda" \
      "$HOME/mambaforge/bin/conda" \
      "$HOME/miniconda3/bin/conda" \
      "$HOME/anaconda3/bin/conda" \
      "/opt/homebrew/Caskroom/miniforge/base/bin/conda" \
      "/opt/homebrew/anaconda3/bin/conda" \
      "/usr/local/Caskroom/miniforge/base/bin/conda" \
      "/opt/miniconda3/bin/conda" \
      "/opt/anaconda3/bin/conda"
    do
      if [ -n "$candidate" ] && [ -x "$candidate" ]; then
        conda_exe="$candidate"
        break
      fi
    done
  fi

  if [ -z "$conda_exe" ]; then
    cat >&2 <<'MSG'
ERROR: conda was not found.

On an HPC, check that the conda module named by CRW_CONDA_MODULE exists.
Otherwise install miniforge (conda-forge's minimal distribution), then re-run:

  macOS, with Homebrew:
    brew install --cask miniforge
    conda init "$(basename "$SHELL")"

  macOS or Linux, without Homebrew:
    curl -L -o miniforge.sh \
      "https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-$(uname)-$(uname -m).sh"
    bash miniforge.sh -b -p "$HOME/miniforge3"
    "$HOME/miniforge3/bin/conda" init "$(basename "$SHELL")"

Then open a new terminal. To manage the environment manually instead,
set CRW_SKIP_CONDA=1.
MSG
    return 1
  fi

  # ---- create the environment if it does not exist ------------------
  # A failure of `conda env list` itself must not be read as "the
  # environment is missing", or a broken install would silently trigger a
  # pointless multi-minute rebuild.
  local env_listing
  if ! env_listing="$("$conda_exe" env list 2>&1)"; then
    echo "ERROR: '$conda_exe env list' failed, so the conda install looks broken:" >&2
    echo "$env_listing" >&2
    return 1
  fi

  if ! printf '%s\n' "$env_listing" | awk '{print $1}' | grep -qx "$env_name"; then
    if [ ! -f "$yml" ]; then
      echo "ERROR: conda environment '$env_name' does not exist and" >&2
      echo "       $yml was not found to create it from." >&2
      return 1
    fi
    echo "conda environment '$env_name' not found; creating it from $yml."
    echo "This downloads Python and its packages and takes a few minutes."
    if command -v mamba >/dev/null 2>&1; then
      solver="$(command -v mamba)"
    else
      solver="$conda_exe"
    fi
    if ! "$solver" env create -n "$env_name" -f "$yml"; then
      echo "ERROR: failed to create the conda environment from $yml." >&2
      return 1
    fi
    echo "Created conda environment '$env_name'."
  fi

  # ---- activate -----------------------------------------------------
  # conda's activation scripts are not written for `set -e` / `set -u`,
  # so the calling shell's options are relaxed here and restored after.
  if [ -n "${ZSH_VERSION:-}" ]; then
    shell_name="zsh"
  else
    shell_name="bash"
  fi

  # Save errexit/nounset via "$-" rather than "$(set +o)": bash turns
  # errexit OFF inside a command-substitution subshell, so the snapshot
  # would read "set +o errexit" and restoring it would silently disable
  # errexit in the calling script.
  case $- in *e*) had_e=1 ;; *) had_e=0 ;; esac
  case $- in *u*) had_u=1 ;; *) had_u=0 ;; esac

  set +eu
  eval "$("$conda_exe" shell."$shell_name" hook 2>/dev/null)"
  conda activate "$env_name"
  rc=$?
  if [ "$had_e" = 1 ]; then set -e; fi
  if [ "$had_u" = 1 ]; then set -u; fi

  if [ "$rc" -ne 0 ] || [ "${CONDA_DEFAULT_ENV:-}" != "$env_name" ]; then
    echo "ERROR: could not activate conda environment '$env_name'." >&2
    echo "       Activate it by hand and re-run:  conda activate $env_name" >&2
    return 1
  fi

  echo "Activated conda environment '$env_name' (${CONDA_PREFIX:-unknown prefix})."
  return 0
}

crw_load_modules && crw_activate_env && crw_check_packages

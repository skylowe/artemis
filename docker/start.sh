#!/bin/bash
# ARTEMIS Container Startup Script
# Sets up per-user workspace, starts Shiny, and launches jupyterhub-singleuser
set -eo pipefail

PROJECT_DIR="/home/artemis/project"
PERSIST_DIR="/home/artemis/persist"
BASELINE_TARGETS="/home/artemis/baseline_targets"
BASELINE_CACHE="/home/artemis/baseline_cache"

echo "=== ARTEMIS container starting ==="

# ============================================================================
# 1. Set up per-user persistent directories
# ============================================================================

mkdir -p "${PERSIST_DIR}/scenarios"
mkdir -p "${PERSIST_DIR}/app_data/baseline"
mkdir -p "${PERSIST_DIR}/app_data/scenarios"

# Copy baseline _targets only on first start (when no _targets exists in persist).
# Subsequent container restarts reuse the cached _targets from prior runs,
# so config changes only rebuild affected targets (~1-2 min) instead of
# a full from-scratch build (~6-7 min).
#
# To force a fresh baseline copy (e.g., after updating host pipeline code),
# delete the persist _targets: rm -rf /home/jupyterhub/users/<user>/persist/_targets
if [ -d "${BASELINE_TARGETS}/meta" ] && [ ! -d "${PERSIST_DIR}/_targets/meta" ]; then
    echo "First start: copying baseline _targets from image..."
    rm -rf "${PERSIST_DIR}/_targets"
    mkdir -p "${PERSIST_DIR}/_targets"
    cp -r "${BASELINE_TARGETS}/." "${PERSIST_DIR}/_targets/" || echo "WARNING: _targets copy failed with exit code $?"
    echo "Baseline _targets copied ($(du -sh ${PERSIST_DIR}/_targets | cut -f1))"
    rm -f "${PERSIST_DIR}/app_data/baseline/baseline_data.rds"
    # Clear stale scenario store so it gets re-copied from fresh baseline
    rm -rf "${PERSIST_DIR}/_targets_scenario"
else
    echo "Using existing _targets from persist volume ($(du -sh ${PERSIST_DIR}/_targets 2>/dev/null | cut -f1 || echo 'empty'))"
fi

# Symlink writable _targets into project directory
if [ ! -e "${PROJECT_DIR}/_targets" ]; then
    ln -s "${PERSIST_DIR}/_targets" "${PROJECT_DIR}/_targets"
fi

# Copy baseline data/cache to per-user persist on first start.
# Each user gets a writable copy so scenario runs that trigger new code paths
# (e.g., different historical population source) can write new cache files.
if [ -d "${BASELINE_CACHE}" ] && [ ! -d "${PERSIST_DIR}/cache" ]; then
    echo "First start: copying baseline data/cache to persist..."
    mkdir -p "${PERSIST_DIR}/cache"
    cp -r "${BASELINE_CACHE}/." "${PERSIST_DIR}/cache/" || echo "WARNING: cache copy failed with exit code $?"
    echo "Baseline cache copied ($(du -sh ${PERSIST_DIR}/cache | cut -f1))"
else
    echo "Using existing cache from persist volume ($(du -sh ${PERSIST_DIR}/cache 2>/dev/null | cut -f1 || echo 'empty'))"
fi

# Symlink writable cache into project directory (replace ro mount)
rm -rf "${PROJECT_DIR}/data/cache"
ln -sf "${PERSIST_DIR}/cache" "${PROJECT_DIR}/data/cache"

# Replace image directories with symlinks to persist volume
# (COPY app/ in Dockerfile creates real dirs that block symlink creation)
rm -rf "${PROJECT_DIR}/app/data/baseline"
rm -rf "${PROJECT_DIR}/app/data/scenarios"
mkdir -p "${PROJECT_DIR}/app/data"
ln -sf "${PERSIST_DIR}/app_data/baseline" "${PROJECT_DIR}/app/data/baseline"
ln -sf "${PERSIST_DIR}/app_data/scenarios" "${PROJECT_DIR}/app/data/scenarios"

# Symlink data/scenarios for config-level scenario storage
if [ ! -e "${PROJECT_DIR}/data/scenarios" ]; then
    mkdir -p "${PROJECT_DIR}/data"
    ln -sf "${PERSIST_DIR}/scenarios" "${PROJECT_DIR}/data/scenarios"
fi

# ============================================================================
# 2. Verify data/raw mount
# ============================================================================
if [ ! -d "${PROJECT_DIR}/data/raw" ] || [ -z "$(ls -A ${PROJECT_DIR}/data/raw 2>/dev/null)" ]; then
    echo "WARNING: data/raw is empty or not mounted. Pipeline will not have source data."
fi

# ============================================================================
# 3. Launch the Shiny app as the ONLY served surface via jhsingle-native-proxy
#
# This REPLACES jupyterhub-singleuser. jhsingle-native-proxy acts as the
# JupyterHub single-user server (does the Hub OAuth handshake and reports
# activity for the idle-culler) but serves only the proxied Shiny app.
# There is no JupyterLab, classic notebook, terminal, or file browser -
# those routes do not exist. It launches the Shiny process itself on
# --destport and waits for it to come up before accepting requests.
# ============================================================================
cd "${PROJECT_DIR}"
echo "Launching ARTEMIS dashboard via jhsingle-native-proxy (kiosk mode)..."

exec jhsingle-native-proxy \
    --ip=0.0.0.0 \
    --port=8888 \
    --destport=3838 \
    --ready-timeout=120 \
    --logs \
    -- \
    R -e "setwd('/home/artemis/project/app'); source('global.R'); source('ui.R'); source('server.R'); shiny::shinyApp(ui=ui, server=server, options=list(host='127.0.0.1', port=3838, launch.browser=FALSE))"

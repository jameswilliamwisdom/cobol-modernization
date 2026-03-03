#!/bin/bash
# RUNJOB.sh — JCL Simulator for Claims Adjudication Pipeline
# Compiles and runs a 3-step pipeline with return code handling.
# Showcases: module compilation (CLMVALID as subprogram),
#            static linking (CLMPROC + CLMVALID), step cascading.

cd "$(dirname "$0")"
mkdir -p data

MAX_RC=0
JOB_LOG="data/JOB.LOG"

# Clear/create the job log
> "$JOB_LOG"

# ---------------------------------------------------------------------------
# Logging
# ---------------------------------------------------------------------------
log() { echo "$(date '+%Y-%m-%d %H:%M:%S') $1" | tee -a "$JOB_LOG"; }

# ---------------------------------------------------------------------------
# COMPILE PHASE — build every program before executing anything
# ---------------------------------------------------------------------------
log "=== COMPILE PHASE ==="

# Step 1: Compile CLMVALID as a module (subprogram)
log "Compiling CLMVALID (module)..."
cobc -m -free -I copybooks CLMVALID.cob 2>&1 | tee -a "$JOB_LOG"
if [ ${PIPESTATUS[0]} -ne 0 ]; then
    log "COMPILE FAILED: CLMVALID"
    log "=== JOB ABORTED - COMPILE ERROR ==="
    exit 12
fi

# Step 2: Compile CLMGEN as standalone executable
log "Compiling CLMGEN..."
cobc -x -free -I copybooks CLMGEN.cob -o CLMGEN 2>&1 | tee -a "$JOB_LOG"
if [ ${PIPESTATUS[0]} -ne 0 ]; then
    log "COMPILE FAILED: CLMGEN"
    log "=== JOB ABORTED - COMPILE ERROR ==="
    exit 12
fi

# Step 3: Compile CLMPROC with CLMVALID statically linked
log "Compiling CLMPROC (with CLMVALID linked)..."
cobc -x -free -I copybooks CLMPROC.cob CLMVALID.cob -o CLMPROC 2>&1 | tee -a "$JOB_LOG"
if [ ${PIPESTATUS[0]} -ne 0 ]; then
    log "COMPILE FAILED: CLMPROC"
    log "=== JOB ABORTED - COMPILE ERROR ==="
    exit 12
fi

# Step 4: Compile CLMRPT as standalone executable
log "Compiling CLMRPT..."
cobc -x -free -I copybooks CLMRPT.cob -o CLMRPT 2>&1 | tee -a "$JOB_LOG"
if [ ${PIPESTATUS[0]} -ne 0 ]; then
    log "COMPILE FAILED: CLMRPT"
    log "=== JOB ABORTED - COMPILE ERROR ==="
    exit 12
fi

log "=== ALL COMPILES SUCCESSFUL ==="

# ---------------------------------------------------------------------------
# EXECUTION PHASE — run each pipeline step with COND logic
# ---------------------------------------------------------------------------
run_step() {
    local STEP_NAME=$1
    local STEP_NUM=$2
    log "--- STEP $STEP_NUM: $STEP_NAME ---"
    if [ $MAX_RC -ge 8 ]; then
        log "STEP $STEP_NUM SKIPPED - MAX_RC=$MAX_RC >= 8"
        return
    fi
    ./"$STEP_NAME" 2>&1 | tee -a "$JOB_LOG"
    RC=${PIPESTATUS[0]}
    log "$STEP_NAME RC=$RC"
    if [ $RC -gt $MAX_RC ]; then
        MAX_RC=$RC
    fi
}

log "=== EXECUTION PHASE ==="
run_step CLMGEN  1
run_step CLMPROC 2
run_step CLMRPT  3

# ---------------------------------------------------------------------------
# FINAL STATUS
# ---------------------------------------------------------------------------
log "=== JOB COMPLETE ==="
log "MAXIMUM RETURN CODE: $MAX_RC"
if [ $MAX_RC -eq 0 ]; then
    log "JOB STATUS: SUCCESS"
elif [ $MAX_RC -le 4 ]; then
    log "JOB STATUS: SUCCESS WITH WARNINGS"
else
    log "JOB STATUS: ERRORS DETECTED"
fi
exit $MAX_RC

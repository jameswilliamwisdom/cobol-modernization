#!/bin/bash
# RUNJOB.sh — JCL Simulator for COBOL Batch Pipeline
# Compiles and runs a 5-step batch pipeline with return code handling.
# Mimics mainframe JCL COND logic: skip remaining steps if MAX_RC >= 8.

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
for prog in GENDATA TXNSORT TXNVALID TXNAPPLY TXNRPT TXNRECNL; do
    log "Compiling $prog..."
    cobc -x -free -I copybooks "${prog}.cob" -o "$prog" 2>&1 | tee -a "$JOB_LOG"
    if [ ${PIPESTATUS[0]} -ne 0 ]; then
        log "COMPILE FAILED: $prog"
        log "=== JOB ABORTED - COMPILE ERROR ==="
        exit 12
    fi
done
log "=== ALL COMPILES SUCCESSFUL ==="

# ---------------------------------------------------------------------------
# DATA GENERATION — always runs first, outside the main pipeline
# ---------------------------------------------------------------------------
log "=== DATA GENERATION ==="
log "Running GENDATA..."
./GENDATA 2>&1 | tee -a "$JOB_LOG"
RC=${PIPESTATUS[0]}
log "GENDATA RC=$RC"
if [ $RC -gt $MAX_RC ]; then
    MAX_RC=$RC
fi

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
run_step TXNSORT  1
run_step TXNVALID 2
run_step TXNAPPLY 3
run_step TXNRPT   4
run_step TXNRECNL 5

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

"""ONNX Runtime's telemetry, kept off.  Imported first by every module that
loads onnxruntime -- qdrant_client does at import, through fastembed.

onnxruntime 1.29 starts Microsoft's telemetry (the 1DS SDK) when it loads,
and about ten seconds later posts it to mobile.events.data.microsoft.com
from a thread of its own.  A process that exits while that post is out
aborts after its work is done: exit's static destructors tear the
telemetry down (~PosixEnv), the post's answer reaches that thread, and it
locks a recursive_mutex exit has already destroyed --

  libc++abi: terminating due to uncaught exception of type
  std::__1::system_error: recursive_mutex lock failed: Invalid argument

-- a pytest run that ended in that second, half the time (ask 72535795).
ORT_DISABLE_TELEMETRY, read once when onnxruntime loads, keeps it from
starting at all: no thread, no post, nothing left to tear down at exit.
Set too late, it does nothing.  setdefault: a value the user set is theirs.
"""

import os

os.environ.setdefault("ORT_DISABLE_TELEMETRY", "1")

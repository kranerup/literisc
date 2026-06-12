"""
asm.py - Bridge to the liteRISC Lisp assembler.

Calls sbcl with the literisc ASDF system to assemble Lisp S-expression
assembly programs into Python byte lists.

Usage:
    from asm import assemble

    # Keyword args become Lisp defparameter bindings accessible in asm_forms.
    bytelist = assemble(
        "(Rx= RESULT_PHYS R1) (Rx= 42 R0) (A=Rx R0) (M[Rx]=A R1) (label done) (j done)",
        RESULT_PHYS=16384,
    )
"""
import os
import subprocess
import tempfile

# Project root: two levels above rtl/literisc/
_LITERISC_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))


def assemble(asm_forms_str, **constants):
    """Assemble Lisp assembly forms into a list of bytes.

    asm_forms_str:
        String of Lisp assembly forms (without outer list parens), e.g.
        "(Rx= 42 R0) (A=Rx R0) (label done) (j done)"

    constants:
        Keyword arguments become Lisp defparameter bindings, e.g.
        RESULT_PHYS=16384  →  (defparameter RESULT_PHYS 16384)
        Names are uppercased; underscores stay as-is (valid Lisp identifiers).

    Returns:
        list of int (byte values 0-255)

    Raises:
        RuntimeError if sbcl fails or returns no output.
    """
    defparams = "\n".join(
        f"(defparameter {name.upper()} {value})"
        for name, value in constants.items()
    )

    # Try to locate quicklisp setup in common locations
    ql_candidates = [
        os.path.expanduser("~/quicklisp/setup.lisp"),
        os.path.expanduser("~/.quicklisp/setup.lisp"),
    ]
    ql_setup = next((p for p in ql_candidates if os.path.exists(p)), None)
    ql_load = f'(load "{ql_setup}")' if ql_setup else ""

    script = f"""\
(require :asdf)
{ql_load}
(pushnew #p"{_LITERISC_ROOT}/" asdf:*central-registry* :test #'equal)
(handler-case
    (asdf:load-system :literisc :verbose nil)
  (error (e)
    (format *error-output* "literisc load error: ~a~%" e)
    (sb-ext:exit :code 1)))
(use-package :lr-asm)
{defparams}
(handler-case
    (let ((program (assemble '({asm_forms_str}))))
      (fresh-line)
      (format t "BYTES:~{{~a ~}}~%" program))
  (error (e)
    (format *error-output* "assembly error: ~a~%" e)
    (sb-ext:exit :code 2)))
"""

    with tempfile.NamedTemporaryFile(
        mode="w", suffix=".lisp", delete=False, dir="/tmp"
    ) as f:
        f.write(script)
        tmpfile = f.name

    try:
        result = subprocess.run(
            ["sbcl", "--script", tmpfile],
            capture_output=True,
            text=True,
            timeout=120,
        )
        if result.returncode != 0:
            raise RuntimeError(
                f"Assembler exited {result.returncode}:\n"
                f"stderr: {result.stderr}\nstdout: {result.stdout}"
            )
        # Find the "BYTES:..." line in stdout (other lines may be Lisp load noise)
        bytes_line = None
        for line in result.stdout.splitlines():
            if line.startswith("BYTES:"):
                bytes_line = line[len("BYTES:"):].strip()
                break
        if bytes_line is None:
            raise RuntimeError(
                f"No BYTES: line in assembler output.\n"
                f"stdout: {result.stdout}\nstderr: {result.stderr}"
            )
        return [int(x) for x in bytes_line.split() if x]
    finally:
        os.unlink(tmpfile)

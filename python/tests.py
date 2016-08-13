"""
Tests (written in Python) for the Haskell jupyter package.

There is a Python test suite because it can utilize the jupyter_client package
(and related) Jupyter packages to ensure that Jupyter communication works as
expected.
"""
import unittest
import subprocess
import os
from contextlib import contextmanager

from jupyter_client.kernelspec import KernelSpecManager
from jupyter_client import MultiKernelManager
import jupyter_client

@contextmanager
def kernel(kernel_name):
    """Start a kernel and provide a client to the kernel.
    Clean up afterward."""
    # Connect to the kernel
    multimanager = MultiKernelManager()
    uid = multimanager.start_kernel(kernel_name)
    manager = multimanager.get_kernel(uid)
    client = manager.client()

    # Prepare the client; don't do anything until it's ready!
    client.start_channels()
    client.wait_for_ready()

    try:
        yield client
    finally:
        # Remove the connection file
        os.remove(client.connection_file)

        # Make sure all kernels turn off.
        multimanager.shutdown_all()


class TestJupyter(unittest.TestCase):
    """Tests for jupyter package."""
    def test_messaging_protocol_version(self):
        """Test that the latest jupyter_client has the expected protocol
        version; this way, when a new version is released, maintainers are
        automatically notified via failing tests."""
        self.assertEqual(jupyter_client.protocol_version, "5.0")

    def test_kernelspec_installed(self):
        """Test that the jupyter kernelspec installations work, and that
        all the examples can be installed as expected."""
        manager = KernelSpecManager()

        # Remove all kernelspecs first.
        subprocess.check_call(["jupyter", "kernelspec", "remove", "-f",
                               "basic", "calculator", "stdin"])

        # Check that kernelspec installation works, the kernel exists after
        # it has been installed, and the executable file is found.
        subprocess.check_call(["kernel-basic", "install"])
        kernelspec = manager.get_kernel_spec("basic")
        self.assertTrue(os.path.isfile(kernelspec.argv[0]))

        subprocess.check_call(["kernel-calculator", "install"])
        kernelspec = manager.get_kernel_spec("calculator")
        self.assertTrue(os.path.isfile(kernelspec.argv[0]))

        subprocess.check_call(["kernel-stdin", "install"])
        kernelspec = manager.get_kernel_spec("stdin")
        self.assertTrue(os.path.isfile(kernelspec.argv[0]))

    def test_basic_kernel_info(self):
        """Test that we can get kernel info from the basic kernel."""
        with kernel("basic") as client:
            # Send the kernel_info_request and get the reply
            request_uuid = client.kernel_info()
            reply = client.get_shell_msg()
            self.assertEqual(reply["content"], {
                'banner': '',
                'help_links': [],
                'implementation': 'Basic',
                'implementation_version': '0.0',
                'language_info':
                {'file_extension': '.txt',
                 'mimetype': 'text/plain',
                 'name': 'Basic',
                 'version': '0.0'},
                'protocol_version': '5.0'
            })
            self.assertEqual(reply["parent_header"]["msg_id"], request_uuid)

    def test_basic_exec(self):
        """Test that we can execute code in the basic kernel."""
        with kernel("basic") as client:
            request_uuid = client.execute("")
            reply = client.get_shell_msg()
            self.assertEqual(reply["header"]["msg_type"], "execute_reply")
            self.assertEqual(reply["content"], {
                "execution_count": 0,
                "status": "ok",
            })
            self.assertEqual(reply["parent_header"]["msg_id"], request_uuid)

    def test_basic_busy_idle_message(self):
        """Test that during code execution, execute input,
        busy, and idle messages are sent."""
        with kernel("basic") as client:
            request_uuid = client.execute("")

            reply = client.get_iopub_msg(timeout=1)
            self.assertEqual(reply["header"]["msg_type"], "status")
            self.assertEqual(reply["content"], {
                "execution_state": "busy",
            })
            self.assertEqual(reply["parent_header"]["msg_id"], request_uuid)

            reply = client.get_iopub_msg(timeout=1)
            self.assertEqual(reply["header"]["msg_type"], "execute_input")
            self.assertEqual(reply["content"], {
                "execution_count": 0,
                "code": "",
            })
            self.assertEqual(reply["parent_header"]["msg_id"], request_uuid)


            reply = client.get_iopub_msg(timeout=1)
            self.assertEqual(reply["header"]["msg_type"], "status")
            self.assertEqual(reply["content"], {
                "execution_state": "idle",
            })
            self.assertEqual(reply["parent_header"]["msg_id"], request_uuid)

    def test_calculator_exec_ok(self):
        """Tests calculator code execution which succeeds."""
        with kernel("calculator") as client:
            client.execute("Compute [] (Lit 1)")
            replies = [client.get_iopub_msg(timeout=1) for _ in range(4)]

            types = [msg["header"]["msg_type"] for msg in replies]
            self.assertEqual(types, ["status", "execute_input",
                                     "display_data", "status"])

            contents = [msg["content"] for msg in replies]
            self.assertEqual(contents, [
                {"execution_state": "busy"},
                {"execution_count": 1, "code": "Compute [] (Lit 1)"},
                {"data": {"text/plain": "1"}, "metadata": {}},
                {"execution_state": "idle"},
            ])

            reply = client.get_shell_msg()
            self.assertEqual(reply["header"]["msg_type"], "execute_reply")
            self.assertEqual(reply["content"], {
                "status": "ok",
                "execution_count": 1,
            })

    def test_calculator_exec_err(self):
        """Tests calculator code execution which fails."""
        with kernel("calculator") as client:
            client.execute("Compute [] (Var 'x')")

            reply = client.get_shell_msg()
            self.assertEqual(reply["header"]["msg_type"], "execute_reply")
            self.assertEqual(reply["content"]["status"], "error")

    def test_calculator_inspect(self):
        """Tests calculator code execution which fails."""
        with kernel("calculator") as client:
            client.inspect("1 + 23 ` Add789", 12)

            reply = client.get_shell_msg()
            self.assertEqual(reply["header"]["msg_type"], "inspect_reply")
            self.assertEqual(reply["content"], {
                "status": "ok",
                "found": True,
                "data": {
                    "text/plain": "Add: Add two expressions."
                },
                "metadata": {},
            })

            client.inspect("1 + 23 ` NotAToken789", 12)

            reply = client.get_shell_msg()
            self.assertEqual(reply["header"]["msg_type"], "inspect_reply")
            self.assertEqual(reply["content"], {
                "status": "ok",
                "found": False,
                "data": {},
                "metadata": {},
            })

    def test_calculator_complete(self):
        """Tests calculator code execution which fails."""
        with kernel("calculator") as client:
            client.complete("1 + 23 ` Comp789", 13)
            reply = client.get_shell_msg()
            self.assertEqual(reply["header"]["msg_type"], "complete_reply")
            self.assertEqual(reply["content"], {
                "status": "ok",
                "cursor_start": 9,
                "cursor_end": 13,
                "matches": ["Compute"],
                "metadata": {},
            })

    def test_stdin(self):
        """Tests calculator code execution which fails."""
        with kernel("stdin") as client:
            client.execute("Prompt")

            stdin_msg = client.get_stdin_msg()
            self.assertEqual(stdin_msg["header"]["msg_type"], "input_request")
            self.assertEqual(stdin_msg["content"], {
                "prompt": "Prompt",
                "password": False,
            })

            client.input("input")

            reply = client.get_shell_msg()
            self.assertEqual(reply["header"]["msg_type"], "execute_reply")
            self.assertEqual(reply["content"], {
                "execution_count": 1,
                "status": "ok",
            })


            iopubs = [client.get_iopub_msg(timeout=1) for _ in range(4)]
            types = [msg["header"]["msg_type"] for msg in iopubs]
            self.assertEqual(types, ["status", "execute_input",
                                     "display_data", "status"])

            contents = [msg["content"] for msg in iopubs]
            self.assertEqual(contents, [
                {"execution_state": "busy"},
                {"execution_count": 1, "code": "Prompt"},
                {"data": {"text/plain": "input"}, "metadata": {}},
                {"execution_state": "idle"},
            ])


if __name__ == '__main__':
    # Make sure all kernelspecs are installed before starting the test suite.
    for spec in ["basic", "calculator", "stdin"]:
        subprocess.check_call(["kernel-{}".format(spec), "install"])

    unittest.main(warnings=[])

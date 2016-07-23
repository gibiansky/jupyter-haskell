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

import warnings
with warnings.catch_warnings():
    warnings.filterwarnings("ignore", category=DeprecationWarning)
    from jupyter_client.kernelspec import KernelSpecManager
    from jupyter_client import MultiKernelManager

@contextmanager
def kernel(kernel_name):
    """Start a kernel and provide a client to the kernel.
    Clean up afterward."""
    # Connect to the kernel
    multimanager = MultiKernelManager()
    uid = multimanager.start_kernel(kernel_name)
    manager = multimanager.get_kernel(uid)
    client = manager.client()

    try:
        yield client
    finally:
        # Remove the connection file
        os.remove(client.connection_file)

        # Make sure all kernels turn off.
        multimanager.shutdown_all()


class TestJupyter(unittest.TestCase):
    """Tests for jupyter package."""

    def test_kernelspec_installed(self):
        """Test that the jupyter kernelspec installations work, and that
        all the examples can be installed as expected."""
        manager = KernelSpecManager()

        # Check that kernelspec installation works, the kernel exists after
        # it has been installed, and the executable file is found.
        subprocess.check_call(["kernel-basic", "install"])
        kernelspec = manager.get_kernel_spec("basic")
        self.assertTrue(os.path.isfile(kernelspec.argv[0]))

        subprocess.check_call(["kernel-calculator", "install"])
        kernelspec = manager.get_kernel_spec("calculator")
        self.assertTrue(os.path.isfile(kernelspec.argv[0]))

    def test_can_get_kernel_info(self):
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

    def test_can_execute_code(self):
        """Test that we can execute code in the basic kernel."""
        with kernel("basic") as client:
            # Send the kernel_info_request and get the reply
            request_uuid = client.execute("")
            reply = client.get_shell_msg()
            self.assertEqual(reply["header"]["msg_type"], "execute_reply")
            self.assertEqual(reply["content"], {
                "execution_count": 0,
                "status": "ok",
            })
            self.assertEqual(reply["parent_header"]["msg_id"], request_uuid)

    def test_sending_busy_idle_message(self):
        """Test that during code execution, busy and idle messages are sent."""
        with kernel("basic") as client:
            # Send the kernel_info_request and get the reply
            request_uuid = client.execute("")
            reply = client.get_iopub_msg(timeout=1)
            self.assertEqual(reply["header"]["msg_type"], "status")
            self.assertEqual(reply["content"], {
                "execution_state": "busy",
            })
            self.assertEqual(reply["parent_header"]["msg_id"], request_uuid)

            reply = client.get_iopub_msg(timeout=1)
            self.assertEqual(reply["header"]["msg_type"], "status")
            self.assertEqual(reply["content"], {
                "execution_state": "idle",
            })
            self.assertEqual(reply["parent_header"]["msg_id"], request_uuid)

if __name__ == '__main__':
    unittest.main(warnings=[])

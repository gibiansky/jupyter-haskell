"""
Tests (written in Python) for the Haskell jupyter package.

There is a Python test suite because it can utilize the jupyter_client package
(and related) Jupyter packages to ensure that Jupyter communication works as
expected.
"""
import unittest
import subprocess
import os

from jupyter_client.kernelspec import KernelSpecManager
from jupyter_client import MultiKernelManager

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

    def test_can_get_kernel_info(self):
        """Test that we can get kernel info from the basic kernel."""
        # Connect to the basic kernel.
        multimanager = MultiKernelManager()
        uid = multimanager.start_kernel("basic")
        manager = multimanager.get_kernel(uid)
        client = manager.client()

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

        # Remove the connection file
        os.remove(client.connection_file)

if __name__ == '__main__':
    unittest.main()

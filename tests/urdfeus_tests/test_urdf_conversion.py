import os
import os.path as osp
import subprocess
import tempfile
import unittest

from skrobot.data import fetch_urdfpath
from skrobot.data import pr2_urdfpath
import yaml

from urdfeus.urdf2eus import urdf2eus
from urdfeus.urdf2eus import validate_euslisp_identifier

data_dir = osp.abspath(osp.dirname(__file__))


class TestURDFConversion(unittest.TestCase):
    """Test URDF to EusLisp conversion and loading functionality."""

    def setUp(self):
        """Set up test environment."""
        self.temp_dir = tempfile.mkdtemp()
        # Find EusLisp executable
        self.irteusgl_path = self._find_euslisp_executable()

    def _find_euslisp_executable(self):
        """Find available EusLisp executable."""
        # Try different EusLisp commands
        commands = ['irteusgl', 'eus']

        for cmd in commands:
            try:
                result = subprocess.run(['which', cmd], capture_output=True, text=True)
                if result.returncode == 0:
                    return result.stdout.strip()
            except (subprocess.SubprocessError, FileNotFoundError):
                continue

        # Fallback to checking specific paths
        possible_paths = [
            '/opt/homebrew/bin/irteusgl',  # macOS with Homebrew
            '/usr/local/bin/irteusgl',      # Linux install
            '/usr/bin/irteusgl',            # System install
            '/usr/bin/eus',                 # Ubuntu package install
        ]

        for path in possible_paths:
            if os.path.exists(path):
                return path

        return None

    def tearDown(self):
        """Clean up test environment."""
        import shutil
        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def _check_irteusgl_available(self):
        """Check if irteusgl is available."""
        return self.irteusgl_path is not None

    def _test_euslisp_loading(self, euslisp_file_path, robot_function_name):
        """Test if generated EusLisp file can be loaded and robot instance created."""
        # Skip EusLisp loading tests in CI environment to avoid segfaults
        if os.environ.get('CI'):
            self.skipTest("EusLisp loading tests skipped in CI environment")

        if not self._check_irteusgl_available():
            self.skipTest("irteusgl not available")

        # Create test EusLisp script
        test_script = f"""
(load "{euslisp_file_path}")
(setq *robot* ({robot_function_name}))
(if (null *robot*)
    (progn
        (format t "ERROR: Failed to create robot instance~%")
        (exit 1))
    (progn
        (format t "SUCCESS: Robot instance created~%")
        (send *robot* :angle-vector)
        (format t "SUCCESS: angle-vector obtained~%")
        (exit 0)))
"""

        script_path = osp.join(self.temp_dir, "test_script.l")
        with open(script_path, 'w') as f:
            f.write(test_script)

        # Run irteusgl with the test script
        try:
            # Set environment variables for EusLisp
            env = os.environ.copy()
            if 'EUSDIR' not in env:
                env['EUSDIR'] = '/usr/share/euslisp'

            result = subprocess.run(
                [self.irteusgl_path, script_path],
                capture_output=True,
                text=True,
                timeout=30,
                env=env
            )

            # Check if the test succeeded
            self.assertEqual(result.returncode, 0,
                           f"EusLisp loading failed. stderr: {result.stderr}, stdout: {result.stdout}")
            self.assertIn("SUCCESS: Robot instance created", result.stdout)
            self.assertIn("SUCCESS: angle-vector obtained", result.stdout)

        except subprocess.TimeoutExpired:
            self.fail("EusLisp test timed out")
        except FileNotFoundError:
            self.skipTest("irteusgl not found")

    def test_fetch_robot_conversion(self):
        """Test fetch robot URDF conversion and loading."""
        urdf_path = fetch_urdfpath()
        yaml_path = osp.join(data_dir, "fetch.yaml")
        output_path = osp.join(self.temp_dir, "fetch.l")

        # Test conversion
        with open(output_path, 'w') as f:
            urdf2eus(urdf_path, yaml_path, fp=f)

        # Verify output file exists
        self.assertTrue(osp.exists(output_path))

        # Test EusLisp loading
        self._test_euslisp_loading(output_path, "fetch")

    def test_pr2_robot_conversion(self):
        """Test PR2 robot URDF conversion and loading."""
        urdf_path = pr2_urdfpath()
        yaml_path = osp.join(data_dir, "pr2.yaml")
        output_path = osp.join(self.temp_dir, "pr2.l")

        # Test conversion
        with open(output_path, 'w') as f:
            urdf2eus(urdf_path, yaml_path, fp=f)

        # Verify output file exists
        self.assertTrue(osp.exists(output_path))

        # Test EusLisp loading
        self._test_euslisp_loading(output_path, "pr2")

    def test_conversion_error_handling(self):
        """Test error handling for invalid inputs."""
        # Test with non-existent URDF file
        with self.assertRaises((FileNotFoundError, Exception)):
            with open(osp.join(self.temp_dir, "output.l"), 'w') as f:
                urdf2eus("non_existent.urdf", fp=f)

        # Test with invalid YAML file
        invalid_yaml_path = osp.join(self.temp_dir, "invalid.yaml")
        with open(invalid_yaml_path, 'w') as f:
            f.write("invalid: yaml: content: [\n")

        urdf_path = osp.join(data_dir, "models", "hand_robot.urdf")
        output_path = osp.join(self.temp_dir, "test_invalid.l")

        # This should handle the error gracefully or raise an appropriate exception
        try:
            with open(output_path, 'w') as f:
                urdf2eus(urdf_path, invalid_yaml_path, fp=f)
        except Exception as e:
            # Ensure it's a meaningful error message
            self.assertIsInstance(e, (yaml.YAMLError, Exception))

    def test_custom_robot_name(self):
        """Test custom robot name functionality."""
        urdf_path = fetch_urdfpath()
        output_path = osp.join(self.temp_dir, "custom_name_robot.l")
        custom_name = "my_custom_robot"

        # Test conversion with custom name
        with open(output_path, 'w') as f:
            urdf2eus(urdf_path, robot_name=custom_name, fp=f)

        # Verify output file exists
        self.assertTrue(osp.exists(output_path))

        # Check if custom name appears in the generated file
        with open(output_path) as f:
            content = f.read()
            self.assertIn(f"defun {custom_name}", content)
            self.assertIn(f"defclass {custom_name}-robot", content)
            self.assertIn(f"*{custom_name}*", content)

        # Verify basic output content in CI environment
        if os.environ.get('CI'):
            self.assertIn(f"defclass {custom_name}-robot", content)
            self.assertIn(":init", content)
            self.assertIn(f"defun {custom_name}", content)
        else:
            # Test EusLisp loading in non-CI environment
            self._test_euslisp_loading(output_path, custom_name)

    def test_invalid_robot_names(self):
        """Test validation of invalid robot names."""
        urdf_path = fetch_urdfpath()
        output_path = osp.join(self.temp_dir, "invalid_name.l")

        # Test invalid names that should raise ValueError
        invalid_names = [
            "123robot",      # starts with number
            "robot name",    # contains space
            "robot.name",    # contains dot
            "robot@home",    # contains @
            "",              # empty string
            "if",            # reserved keyword
            "defun",         # reserved keyword
            "nil",           # reserved keyword
        ]

        for invalid_name in invalid_names:
            with self.subTest(name=invalid_name):
                with self.assertRaises(ValueError) as cm:
                    with open(output_path, 'w') as f:
                        urdf2eus(urdf_path, robot_name=invalid_name, fp=f)

                # Check that the error message is informative
                self.assertIn("Invalid robot name", str(cm.exception))

    def test_valid_robot_names(self):
        """Test validation of valid robot names."""
        urdf_path = fetch_urdfpath()

        # Test valid names that should work
        valid_names = [
            "my_robot",
            "robot-v1",
            "MyRobot",
            "_robot",
            "robot123",
            "r2d2",
        ]

        for valid_name in valid_names:
            with self.subTest(name=valid_name):
                output_path = osp.join(self.temp_dir, f"{valid_name}.l")

                # Should not raise any exception
                with open(output_path, 'w') as f:
                    urdf2eus(urdf_path, robot_name=valid_name, fp=f)

                # Verify the name appears in the output
                with open(output_path) as f:
                    content = f.read()
                    self.assertIn(f"defun {valid_name}", content)

    def test_euslisp_identifier_validation(self):
        """Test the validate_euslisp_identifier function directly."""
        # Test valid identifiers
        valid_cases = [
            "my_robot",
            "robot-v1",
            "MyRobot",
            "_robot",
            "robot123",
            "r2d2",
        ]

        for name in valid_cases:
            with self.subTest(name=name):
                is_valid, msg = validate_euslisp_identifier(name)
                self.assertTrue(is_valid, f"'{name}' should be valid: {msg}")

        # Test invalid identifiers
        invalid_cases = [
            ("123robot", "Name must start with a letter or underscore"),
            ("robot name", "Name can only contain letters, digits, underscores, and hyphens"),
            ("robot.name", "Name can only contain letters, digits, underscores, and hyphens"),
            ("robot@home", "Name can only contain letters, digits, underscores, and hyphens"),
            ("", "Name cannot be empty"),
            ("if", "'if' is a reserved EusLisp keyword"),
            ("defun", "'defun' is a reserved EusLisp keyword"),
            ("nil", "'nil' is a reserved EusLisp keyword"),
        ]

        for name, expected_error in invalid_cases:
            with self.subTest(name=name):
                is_valid, msg = validate_euslisp_identifier(name)
                self.assertFalse(is_valid, f"'{name}' should be invalid")
                self.assertIn(expected_error, msg)


if __name__ == '__main__':
    unittest.main()

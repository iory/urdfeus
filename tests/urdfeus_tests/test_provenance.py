import subprocess
import sys
import unittest

from urdfeus import provenance


class TestPackageReports(unittest.TestCase):

    def test_every_reported_package_gets_a_row(self):
        names = [name for name, _version, _path in provenance.package_reports()]
        self.assertEqual(names, list(provenance.REPORTED_PACKAGES))

    def test_a_row_carries_the_path_it_was_imported_from(self):
        """The path is the point: a version number alone cannot tell a shim in
        ``~/.local/bin`` apart from a checkout on ``sys.path``."""
        rows = {name: path for name, _version, path in provenance.package_reports()}
        self.assertIn('urdfeus', rows['urdfeus'])

    def test_a_package_that_will_not_import_is_reported_not_raised(self):
        original = provenance.REPORTED_PACKAGES
        provenance.REPORTED_PACKAGES = ('urdfeus_no_such_package',)
        try:
            (name, version, path), = provenance.package_reports()
        finally:
            provenance.REPORTED_PACKAGES = original
        self.assertEqual(name, 'urdfeus_no_such_package')
        self.assertEqual(version, 'not importable')
        self.assertIn('Error', path)


class TestPathVariableSummary(unittest.TestCase):

    def test_a_short_value_is_kept_whole(self):
        self.assertEqual(
            provenance._summarise_path_variable('/opt/ros/one'), '/opt/ros/one')

    def test_an_unset_variable_says_so(self):
        self.assertEqual(provenance._summarise_path_variable(None), '(unset)')

    def test_a_sourced_workspace_is_summarised(self):
        """Dozens of entries in one variable would bury the rest of the
        report, and the count plus the first entry is what identifies it."""
        value = ':'.join(f'/ws/install/pkg{i}' for i in range(40))
        summary = provenance._summarise_path_variable(value)
        self.assertIn('40 entries', summary)
        self.assertIn('/ws/install/pkg0', summary)
        self.assertLess(len(summary), len(value))


class TestDoctorReport(unittest.TestCase):

    def test_it_names_the_interpreter_and_the_packages(self):
        report = provenance.doctor_report()
        self.assertIn(sys.executable, report)
        for name in provenance.REPORTED_PACKAGES:
            self.assertIn(name + ':', report)

    def test_it_reports_the_ros_search_path(self):
        self.assertIn('ROS_PACKAGE_PATH', provenance.doctor_report())


class TestCommandLineFlags(unittest.TestCase):

    COMMANDS = ('urdf2eus', 'mesh2eus', 'eus2urdf')

    def _run(self, command, *flags):
        return subprocess.run(
            [sys.executable, '-m', 'urdfeus.apps.' + command, *flags],
            capture_output=True, text=True)

    def test_version_prints_the_version(self):
        for command in self.COMMANDS:
            result = self._run(command, '--version')
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertIn('urdfeus', result.stdout)

    def test_doctor_works_without_the_positional_arguments(self):
        """A report is most needed when a conversion cannot even start, so
        ``--doctor`` must not demand the paths it is meant to explain."""
        for command in self.COMMANDS:
            result = self._run(command, '--doctor')
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertIn('python:', result.stdout)

    def test_the_positional_arguments_are_still_required(self):
        for command in self.COMMANDS:
            result = self._run(command)
            self.assertNotEqual(result.returncode, 0)
            self.assertIn('required', result.stderr)


if __name__ == '__main__':
    unittest.main()

#!/usr/bin/env python
"""
Veyra Package Manager (veyra-pm)
Simple package manager for Veyra libraries
"""

import os
import sys
import urllib.request
import json
import shutil

class VeyraPM:
    def __init__(self):
        self.lib_dir = os.path.join(os.path.dirname(__file__), 'lib')
        if not os.path.exists(self.lib_dir):
            os.makedirs(self.lib_dir)

    def install(self, package_name):
        """Install a package from the registry"""
        print("Installing {}...".format(package_name))

        
        if package_name == 'math':
            self._download_package('https://raw.githubusercontent.com/veyra/math/main/math.veyra', 'math.veyra')
        elif package_name == 'web':
            self._download_package('https://raw.githubusercontent.com/veyra/web/main/web.veyra', 'web.veyra')
        else:
            print("Package {} not found".format(package_name))
            return False

        print("Package {} installed successfully".format(package_name))
        return True

    def _download_package(self, url, filename):
        """Download a package file"""
        try:
            response = urllib.request.urlopen(url)
            content = response.read()
            with open(os.path.join(self.lib_dir, filename), 'w') as f:
                f.write(content)
        except Exception as e:
            print("Error downloading package: {}".format(e))
            return False
        return True

    def list_packages(self):
        """List installed packages"""
        if not os.path.exists(self.lib_dir):
            print("No packages installed")
            return

        files = os.listdir(self.lib_dir)
        if not files:
            print("No packages installed")
        else:
            print("Installed packages:")
            for f in files:
                if f.endswith('.veyra'):
                    print("  - {}".format(f[:-6]))

def main():
    if len(sys.argv) < 2:
        print("Usage: python veyra-pm.py <command> [package]")
        print("Commands:")
        print("  install <package>  - Install a package")
        print("  list               - List installed packages")
        return

    pm = VeyraPM()
    command = sys.argv[1]

    if command == 'install' and len(sys.argv) > 2:
        pm.install(sys.argv[2])
    elif command == 'list':
        pm.list_packages()
    else:
        print("Unknown command")

if __name__ == '__main__':
    main()
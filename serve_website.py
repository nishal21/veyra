#!/usr/bin/env python3
"""
Simple HTTP server to serve the Veyra website locally.
Run this script and visit http://localhost:8000 to view the website.
"""

import http.server
import socketserver
import os
from pathlib import Path

class CustomHTTPRequestHandler(http.server.SimpleHTTPRequestHandler):
    def do_GET(self):
        # Set the directory to serve from
        self.directory = str(Path(__file__).parent / "web")

        # Handle root path
        if self.path == '/':
            self.path = '/index.html'

        # Call parent method
        return super().do_GET()

    def end_headers(self):
        # Add CORS headers for development
        self.send_header('Access-Control-Allow-Origin', '*')
        self.send_header('Access-Control-Allow-Methods', 'GET')
        self.send_header('Cache-Control', 'no-cache')
        super().end_headers()

def main():
    # Change to the project root directory
    os.chdir(Path(__file__).parent)

    # Set up server
    PORT = 8080
    Handler = CustomHTTPRequestHandler

    with socketserver.TCPServer(("", PORT), Handler) as httpd:
        print(f"🚀 Veyra Website Server Started!")
        print(f"📱 Visit: http://localhost:{PORT}")
        print(f"🎨 Serving from: {Path(__file__).parent / 'web'}")
        print("Press Ctrl+C to stop the server")

        try:
            httpd.serve_forever()
        except KeyboardInterrupt:
            print("\n👋 Server stopped. Goodbye!")

if __name__ == "__main__":
    main()
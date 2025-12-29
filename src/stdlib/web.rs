//! Veyra HTTP and Web Server
//! 
//! Built-in HTTP client and server for web development.

use std::collections::HashMap;
use std::io::{Read, Write, BufRead, BufReader};
use std::net::{TcpListener, TcpStream};
use std::sync::Arc;

/// HTTP Response
pub struct Response {
    pub status: u16,
    pub status_text: String,
    pub headers: HashMap<String, String>,
    pub body: String,
}

impl Response {
    pub fn ok(body: String) -> Self {
        Self {
            status: 200,
            status_text: "OK".to_string(),
            headers: HashMap::new(),
            body,
        }
    }
    
    pub fn not_found() -> Self {
        Self {
            status: 404,
            status_text: "Not Found".to_string(),
            headers: HashMap::new(),
            body: "404 Not Found".to_string(),
        }
    }
    
    pub fn error(message: String) -> Self {
        Self {
            status: 500,
            status_text: "Internal Server Error".to_string(),
            headers: HashMap::new(),
            body: message,
        }
    }
    
    pub fn with_header(mut self, key: &str, value: &str) -> Self {
        self.headers.insert(key.to_string(), value.to_string());
        self
    }
    
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut response = format!(
            "HTTP/1.1 {} {}\r\n",
            self.status,
            self.status_text
        );
        
        // Add content-length
        response.push_str(&format!("Content-Length: {}\r\n", self.body.len()));
        
        // Add headers
        for (key, value) in &self.headers {
            response.push_str(&format!("{}: {}\r\n", key, value));
        }
        
        // Add body
        response.push_str("\r\n");
        response.push_str(&self.body);
        
        response.into_bytes()
    }
}

/// HTTP Request
pub struct Request {
    pub method: String,
    pub path: String,
    pub headers: HashMap<String, String>,
    pub body: String,
    pub query_params: HashMap<String, String>,
}

impl Request {
    pub fn parse(stream: &mut TcpStream) -> Result<Self, String> {
        let mut reader = BufReader::new(stream.try_clone().map_err(|e| e.to_string())?);
        
        // Read request line
        let mut request_line = String::new();
        reader.read_line(&mut request_line).map_err(|e| e.to_string())?;
        
        let parts: Vec<&str> = request_line.trim().split_whitespace().collect();
        if parts.len() < 2 {
            return Err("Invalid request line".to_string());
        }
        
        let method = parts[0].to_string();
        let full_path = parts[1];
        
        // Parse path and query params
        let (path, query_params) = if let Some(qmark) = full_path.find('?') {
            let path = full_path[..qmark].to_string();
            let query_str = &full_path[qmark + 1..];
            let params: HashMap<String, String> = query_str
                .split('&')
                .filter_map(|pair| {
                    let mut parts = pair.splitn(2, '=');
                    Some((
                        parts.next()?.to_string(),
                        parts.next().unwrap_or("").to_string(),
                    ))
                })
                .collect();
            (path, params)
        } else {
            (full_path.to_string(), HashMap::new())
        };
        
        // Read headers
        let mut headers = HashMap::new();
        loop {
            let mut line = String::new();
            reader.read_line(&mut line).map_err(|e| e.to_string())?;
            let line = line.trim();
            
            if line.is_empty() {
                break;
            }
            
            if let Some(colon) = line.find(':') {
                let key = line[..colon].trim().to_lowercase();
                let value = line[colon + 1..].trim().to_string();
                headers.insert(key, value);
            }
        }
        
        // Read body if content-length is present
        let body = if let Some(len_str) = headers.get("content-length") {
            let len: usize = len_str.parse().unwrap_or(0);
            let mut body = vec![0u8; len];
            reader.read_exact(&mut body).map_err(|e| e.to_string())?;
            String::from_utf8_lossy(&body).to_string()
        } else {
            String::new()
        };
        
        Ok(Request {
            method,
            path,
            headers,
            body,
            query_params,
        })
    }
}

/// Route handler type
pub type RouteHandler = Arc<dyn Fn(&Request) -> Response + Send + Sync>;

/// HTTP Server
pub struct Server {
    routes: HashMap<String, RouteHandler>,
    static_content: Option<String>,
}

impl Server {
    pub fn new() -> Self {
        Self {
            routes: HashMap::new(),
            static_content: None,
        }
    }
    
    pub fn route<F>(&mut self, path: &str, handler: F)
    where
        F: Fn(&Request) -> Response + Send + Sync + 'static,
    {
        self.routes.insert(path.to_string(), Arc::new(handler));
    }
    
    pub fn get<F>(&mut self, path: &str, handler: F)
    where
        F: Fn(&Request) -> Response + Send + Sync + 'static,
    {
        self.route(path, handler);
    }
    
    pub fn post<F>(&mut self, path: &str, handler: F)
    where
        F: Fn(&Request) -> Response + Send + Sync + 'static,
    {
        self.route(path, handler);
    }
    
    pub fn static_content(&mut self, html: String) {
        self.static_content = Some(html);
    }
    
    pub fn serve(&self, port: u16) -> Result<(), String> {
        let addr = format!("0.0.0.0:{}", port);
        let listener = TcpListener::bind(&addr).map_err(|e| e.to_string())?;
        
        println!("🚀 Veyra server running at http://localhost:{}", port);
        
        for stream in listener.incoming() {
            match stream {
                Ok(mut stream) => {
                    self.handle_connection(&mut stream);
                }
                Err(e) => {
                    eprintln!("Connection error: {}", e);
                }
            }
        }
        
        Ok(())
    }
    
    fn handle_connection(&self, stream: &mut TcpStream) {
        let request = match Request::parse(stream) {
            Ok(req) => req,
            Err(e) => {
                let _ = stream.write_all(&Response::error(e).to_bytes());
                return;
            }
        };
        
        let response = if let Some(handler) = self.routes.get(&request.path) {
            handler(&request)
        } else if request.path == "/" {
            if let Some(ref content) = self.static_content {
                Response::ok(content.clone())
                    .with_header("Content-Type", "text/html; charset=utf-8")
            } else {
                Response::not_found()
            }
        } else {
            Response::not_found()
        };
        
        let _ = stream.write_all(&response.to_bytes());
    }
}

impl Default for Server {
    fn default() -> Self {
        Self::new()
    }
}

/// HTTP Client functions
pub mod client {
    use super::*;
    use std::net::TcpStream;
    
    /// Simple HTTP GET request
    pub fn get(url: &str) -> Result<String, String> {
        let (host, port, path) = parse_url(url)?;
        
        let mut stream = TcpStream::connect(format!("{}:{}", host, port))
            .map_err(|e| e.to_string())?;
        
        let request = format!(
            "GET {} HTTP/1.1\r\nHost: {}\r\nConnection: close\r\n\r\n",
            path, host
        );
        
        stream.write_all(request.as_bytes()).map_err(|e| e.to_string())?;
        
        let mut response = String::new();
        stream.read_to_string(&mut response).map_err(|e| e.to_string())?;
        
        // Extract body (after \r\n\r\n)
        if let Some(pos) = response.find("\r\n\r\n") {
            Ok(response[pos + 4..].to_string())
        } else {
            Ok(response)
        }
    }
    
    /// Simple HTTP POST request
    pub fn post(url: &str, body: &str) -> Result<String, String> {
        let (host, port, path) = parse_url(url)?;
        
        let mut stream = TcpStream::connect(format!("{}:{}", host, port))
            .map_err(|e| e.to_string())?;
        
        let request = format!(
            "POST {} HTTP/1.1\r\nHost: {}\r\nContent-Length: {}\r\nContent-Type: application/json\r\nConnection: close\r\n\r\n{}",
            path, host, body.len(), body
        );
        
        stream.write_all(request.as_bytes()).map_err(|e| e.to_string())?;
        
        let mut response = String::new();
        stream.read_to_string(&mut response).map_err(|e| e.to_string())?;
        
        // Extract body
        if let Some(pos) = response.find("\r\n\r\n") {
            Ok(response[pos + 4..].to_string())
        } else {
            Ok(response)
        }
    }
    
    fn parse_url(url: &str) -> Result<(String, u16, String), String> {
        let url = url.trim_start_matches("http://").trim_start_matches("https://");
        
        let (host_port, path) = if let Some(slash) = url.find('/') {
            (&url[..slash], &url[slash..])
        } else {
            (url, "/")
        };
        
        let (host, port) = if let Some(colon) = host_port.find(':') {
            let port: u16 = host_port[colon + 1..].parse().map_err(|_| "Invalid port")?;
            (&host_port[..colon], port)
        } else {
            (host_port, 80)
        };
        
        Ok((host.to_string(), port, path.to_string()))
    }
}

/// Create an HTML page
pub fn create_page(title: &str, body: &str, css: &str) -> String {
    format!(
        r#"<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{}</title>
    <style>{}</style>
</head>
<body>
    {}
</body>
</html>"#,
        title, css, body
    )
}

/// Create an HTML element
pub fn html_element(tag: &str, content: &str, attrs: &HashMap<String, String>) -> String {
    let attrs_str: String = attrs
        .iter()
        .map(|(k, v)| format!(" {}=\"{}\"", k, v))
        .collect();
    
    if tag == "input" || tag == "img" || tag == "br" || tag == "hr" {
        format!("<{}{} />", tag, attrs_str)
    } else {
        format!("<{}{}>{}</{}>", tag, attrs_str, content, tag)
    }
}

/// Serve a static page
pub fn serve_page(content: String, port: u16) {
    let mut server = Server::new();
    server.static_content(content);
    let _ = server.serve(port);
}

//! Veyra App Development Library
//! 
//! Cross-platform UI and application development framework.
//! Works for desktop, mobile, and web applications.

use std::collections::HashMap;

/// Application component types
#[derive(Debug, Clone)]
pub enum ComponentType {
    Container,
    Text,
    Button,
    Input,
    Image,
    List,
    Grid,
    Card,
    Modal,
    Navigation,
    Form,
    Custom(String),
}

/// Style properties for components
#[derive(Debug, Clone, Default)]
pub struct Style {
    // Layout
    pub width: Option<String>,
    pub height: Option<String>,
    pub margin: Option<String>,
    pub padding: Option<String>,
    pub display: Option<String>,
    pub flex_direction: Option<String>,
    pub justify_content: Option<String>,
    pub align_items: Option<String>,
    pub gap: Option<String>,
    
    // Colors
    pub background: Option<String>,
    pub color: Option<String>,
    pub border: Option<String>,
    pub border_radius: Option<String>,
    pub box_shadow: Option<String>,
    
    // Typography
    pub font_size: Option<String>,
    pub font_weight: Option<String>,
    pub font_family: Option<String>,
    pub text_align: Option<String>,
    
    // Effects
    pub opacity: Option<String>,
    pub transform: Option<String>,
    pub transition: Option<String>,
    pub cursor: Option<String>,
}

impl Style {
    pub fn new() -> Self {
        Self::default()
    }
    
    pub fn to_css(&self) -> String {
        let mut css = String::new();
        
        if let Some(v) = &self.width { css.push_str(&format!("width: {}; ", v)); }
        if let Some(v) = &self.height { css.push_str(&format!("height: {}; ", v)); }
        if let Some(v) = &self.margin { css.push_str(&format!("margin: {}; ", v)); }
        if let Some(v) = &self.padding { css.push_str(&format!("padding: {}; ", v)); }
        if let Some(v) = &self.display { css.push_str(&format!("display: {}; ", v)); }
        if let Some(v) = &self.flex_direction { css.push_str(&format!("flex-direction: {}; ", v)); }
        if let Some(v) = &self.justify_content { css.push_str(&format!("justify-content: {}; ", v)); }
        if let Some(v) = &self.align_items { css.push_str(&format!("align-items: {}; ", v)); }
        if let Some(v) = &self.gap { css.push_str(&format!("gap: {}; ", v)); }
        if let Some(v) = &self.background { css.push_str(&format!("background: {}; ", v)); }
        if let Some(v) = &self.color { css.push_str(&format!("color: {}; ", v)); }
        if let Some(v) = &self.border { css.push_str(&format!("border: {}; ", v)); }
        if let Some(v) = &self.border_radius { css.push_str(&format!("border-radius: {}; ", v)); }
        if let Some(v) = &self.box_shadow { css.push_str(&format!("box-shadow: {}; ", v)); }
        if let Some(v) = &self.font_size { css.push_str(&format!("font-size: {}; ", v)); }
        if let Some(v) = &self.font_weight { css.push_str(&format!("font-weight: {}; ", v)); }
        if let Some(v) = &self.font_family { css.push_str(&format!("font-family: {}; ", v)); }
        if let Some(v) = &self.text_align { css.push_str(&format!("text-align: {}; ", v)); }
        if let Some(v) = &self.opacity { css.push_str(&format!("opacity: {}; ", v)); }
        if let Some(v) = &self.transform { css.push_str(&format!("transform: {}; ", v)); }
        if let Some(v) = &self.transition { css.push_str(&format!("transition: {}; ", v)); }
        if let Some(v) = &self.cursor { css.push_str(&format!("cursor: {}; ", v)); }
        
        css
    }
}

/// Builder pattern for styles
impl Style {
    pub fn width(mut self, v: &str) -> Self { self.width = Some(v.to_string()); self }
    pub fn height(mut self, v: &str) -> Self { self.height = Some(v.to_string()); self }
    pub fn margin(mut self, v: &str) -> Self { self.margin = Some(v.to_string()); self }
    pub fn padding(mut self, v: &str) -> Self { self.padding = Some(v.to_string()); self }
    pub fn background(mut self, v: &str) -> Self { self.background = Some(v.to_string()); self }
    pub fn color(mut self, v: &str) -> Self { self.color = Some(v.to_string()); self }
    pub fn border(mut self, v: &str) -> Self { self.border = Some(v.to_string()); self }
    pub fn border_radius(mut self, v: &str) -> Self { self.border_radius = Some(v.to_string()); self }
    pub fn font_size(mut self, v: &str) -> Self { self.font_size = Some(v.to_string()); self }
    pub fn flex(mut self) -> Self { self.display = Some("flex".to_string()); self }
    pub fn flex_row(mut self) -> Self { self.flex_direction = Some("row".to_string()); self }
    pub fn flex_col(mut self) -> Self { self.flex_direction = Some("column".to_string()); self }
    pub fn center(mut self) -> Self {
        self.justify_content = Some("center".to_string());
        self.align_items = Some("center".to_string());
        self
    }
}

/// UI Component
#[derive(Debug, Clone)]
pub struct Component {
    pub id: String,
    pub component_type: ComponentType,
    pub content: String,
    pub style: Style,
    pub attributes: HashMap<String, String>,
    pub children: Vec<Component>,
    pub events: HashMap<String, String>,
}

impl Component {
    pub fn new(component_type: ComponentType) -> Self {
        Self {
            id: generate_id(),
            component_type,
            content: String::new(),
            style: Style::new(),
            attributes: HashMap::new(),
            children: Vec::new(),
            events: HashMap::new(),
        }
    }
    
    pub fn with_id(mut self, id: &str) -> Self {
        self.id = id.to_string();
        self
    }
    
    pub fn with_content(mut self, content: &str) -> Self {
        self.content = content.to_string();
        self
    }
    
    pub fn with_style(mut self, style: Style) -> Self {
        self.style = style;
        self
    }
    
    pub fn with_child(mut self, child: Component) -> Self {
        self.children.push(child);
        self
    }
    
    pub fn with_children(mut self, children: Vec<Component>) -> Self {
        self.children = children;
        self
    }
    
    pub fn with_attr(mut self, key: &str, value: &str) -> Self {
        self.attributes.insert(key.to_string(), value.to_string());
        self
    }
    
    pub fn on_click(mut self, handler: &str) -> Self {
        self.events.insert("click".to_string(), handler.to_string());
        self
    }
    
    pub fn on_change(mut self, handler: &str) -> Self {
        self.events.insert("change".to_string(), handler.to_string());
        self
    }
    
    /// Render component to HTML
    pub fn to_html(&self) -> String {
        let tag = match &self.component_type {
            ComponentType::Container => "div",
            ComponentType::Text => "span",
            ComponentType::Button => "button",
            ComponentType::Input => "input",
            ComponentType::Image => "img",
            ComponentType::List => "ul",
            ComponentType::Grid => "div",
            ComponentType::Card => "div",
            ComponentType::Modal => "div",
            ComponentType::Navigation => "nav",
            ComponentType::Form => "form",
            ComponentType::Custom(s) => s.as_str(),
        };
        
        let mut attrs = format!("id=\"{}\"", self.id);
        
        // Add style
        let style_css = self.style.to_css();
        if !style_css.is_empty() {
            attrs.push_str(&format!(" style=\"{}\"", style_css));
        }
        
        // Add attributes
        for (key, value) in &self.attributes {
            attrs.push_str(&format!(" {}=\"{}\"", key, value));
        }
        
        // Add events
        for (event, handler) in &self.events {
            attrs.push_str(&format!(" on{}=\"{}\"", event, handler));
        }
        
        // Self-closing tags
        if matches!(self.component_type, ComponentType::Input | ComponentType::Image) {
            return format!("<{} {} />", tag, attrs);
        }
        
        // Build children
        let children_html: String = self.children.iter()
            .map(|c| c.to_html())
            .collect::<Vec<_>>()
            .join("\n");
        
        format!("<{} {}>{}{}</{}>", tag, attrs, self.content, children_html, tag)
    }
}

fn generate_id() -> String {
    use std::time::{SystemTime, UNIX_EPOCH};
    let seed = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_nanos();
    format!("veyra-{:x}", seed % 0xFFFFFF)
}

/// Pre-built component helpers
pub mod components {
    use super::*;
    
    pub fn container() -> Component {
        Component::new(ComponentType::Container)
    }
    
    pub fn text(content: &str) -> Component {
        Component::new(ComponentType::Text).with_content(content)
    }
    
    pub fn button(label: &str) -> Component {
        Component::new(ComponentType::Button)
            .with_content(label)
            .with_style(Style::new()
                .padding("10px 20px")
                .border_radius("8px")
                .border("none")
                .background("#7c3aed")
                .color("white")
            )
    }
    
    pub fn input(placeholder: &str) -> Component {
        Component::new(ComponentType::Input)
            .with_attr("placeholder", placeholder)
            .with_style(Style::new()
                .padding("10px")
                .border("1px solid #ccc")
                .border_radius("4px")
            )
    }
    
    pub fn image(src: &str, alt: &str) -> Component {
        Component::new(ComponentType::Image)
            .with_attr("src", src)
            .with_attr("alt", alt)
    }
    
    pub fn card() -> Component {
        Component::new(ComponentType::Card)
            .with_style(Style::new()
                .padding("20px")
                .border_radius("12px")
                .background("white")
            )
    }
    
    pub fn row() -> Component {
        Component::new(ComponentType::Container)
            .with_style(Style::new().flex().flex_row())
    }
    
    pub fn column() -> Component {
        Component::new(ComponentType::Container)
            .with_style(Style::new().flex().flex_col())
    }
    
    pub fn center() -> Component {
        Component::new(ComponentType::Container)
            .with_style(Style::new().flex().center())
    }
    
    pub fn heading(text: &str, level: u8) -> Component {
        Component::new(ComponentType::Custom(format!("h{}", level)))
            .with_content(text)
    }
    
    pub fn link(text: &str, href: &str) -> Component {
        Component::new(ComponentType::Custom("a".to_string()))
            .with_content(text)
            .with_attr("href", href)
    }
    
    pub fn list(items: Vec<&str>) -> Component {
        let children: Vec<Component> = items.iter()
            .map(|item| Component::new(ComponentType::Custom("li".to_string())).with_content(item))
            .collect();
        
        Component::new(ComponentType::List).with_children(children)
    }
    
    pub fn grid(cols: usize) -> Component {
        let mut style = Style::new();
        style.display = Some("grid".to_string());
        Component::new(ComponentType::Grid).with_style(style)
    }
}

/// Application state management
pub struct State<T> {
    value: T,
    subscribers: Vec<Box<dyn Fn(&T)>>,
}

impl<T: Clone> State<T> {
    pub fn new(initial: T) -> Self {
        Self {
            value: initial,
            subscribers: Vec::new(),
        }
    }
    
    pub fn get(&self) -> T {
        self.value.clone()
    }
    
    pub fn set(&mut self, value: T) {
        self.value = value;
        self.notify();
    }
    
    pub fn update<F>(&mut self, f: F) where F: FnOnce(&T) -> T {
        self.value = f(&self.value);
        self.notify();
    }
    
    fn notify(&self) {
        for subscriber in &self.subscribers {
            subscriber(&self.value);
        }
    }
}

/// Router for single-page applications
pub struct Router {
    routes: HashMap<String, Box<dyn Fn() -> Component>>,
    current: String,
}

impl Router {
    pub fn new() -> Self {
        Self {
            routes: HashMap::new(),
            current: "/".to_string(),
        }
    }
    
    pub fn route<F>(mut self, path: &str, handler: F) -> Self 
    where F: Fn() -> Component + 'static
    {
        self.routes.insert(path.to_string(), Box::new(handler));
        self
    }
    
    pub fn navigate(&mut self, path: &str) {
        self.current = path.to_string();
    }
    
    pub fn render(&self) -> Component {
        if let Some(handler) = self.routes.get(&self.current) {
            handler()
        } else {
            // 404 page
            Component::new(ComponentType::Container)
                .with_child(components::heading("404 - Page Not Found", 1))
        }
    }
}

impl Default for Router {
    fn default() -> Self {
        Self::new()
    }
}

/// Application builder
pub struct App {
    title: String,
    root: Component,
    styles: String,
    scripts: String,
}

impl App {
    pub fn new(title: &str) -> Self {
        Self {
            title: title.to_string(),
            root: Component::new(ComponentType::Container),
            styles: String::new(),
            scripts: String::new(),
        }
    }
    
    pub fn with_root(mut self, component: Component) -> Self {
        self.root = component;
        self
    }
    
    pub fn with_styles(mut self, css: &str) -> Self {
        self.styles = css.to_string();
        self
    }
    
    pub fn with_scripts(mut self, js: &str) -> Self {
        self.scripts = js.to_string();
        self
    }
    
    /// Build to HTML string
    pub fn build(&self) -> String {
        format!(r#"<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{}</title>
    <style>
        * {{ margin: 0; padding: 0; box-sizing: border-box; }}
        body {{ font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; }}
        {}
    </style>
</head>
<body>
    {}
    <script>{}</script>
</body>
</html>"#,
            self.title,
            self.styles,
            self.root.to_html(),
            self.scripts
        )
    }
    
    /// Deploy to web server
    pub fn serve(self, port: u16) {
        use super::web;
        let html = self.build();
        web::serve_page(html, port);
    }
}

/// Theme system
pub struct Theme {
    pub colors: HashMap<String, String>,
    pub fonts: HashMap<String, String>,
    pub spacing: HashMap<String, String>,
    pub borders: HashMap<String, String>,
}

impl Theme {
    pub fn new() -> Self {
        Self {
            colors: HashMap::new(),
            fonts: HashMap::new(),
            spacing: HashMap::new(),
            borders: HashMap::new(),
        }
    }
    
    /// Default dark theme
    pub fn dark() -> Self {
        let mut theme = Self::new();
        theme.colors.insert("background".to_string(), "#1a1a2e".to_string());
        theme.colors.insert("surface".to_string(), "#16213e".to_string());
        theme.colors.insert("primary".to_string(), "#7c3aed".to_string());
        theme.colors.insert("secondary".to_string(), "#00d4ff".to_string());
        theme.colors.insert("text".to_string(), "#e8e8e8".to_string());
        theme.colors.insert("text-muted".to_string(), "#8b8b9a".to_string());
        theme
    }
    
    /// Default light theme
    pub fn light() -> Self {
        let mut theme = Self::new();
        theme.colors.insert("background".to_string(), "#ffffff".to_string());
        theme.colors.insert("surface".to_string(), "#f5f5f5".to_string());
        theme.colors.insert("primary".to_string(), "#7c3aed".to_string());
        theme.colors.insert("secondary".to_string(), "#0ea5e9".to_string());
        theme.colors.insert("text".to_string(), "#1f2937".to_string());
        theme.colors.insert("text-muted".to_string(), "#6b7280".to_string());
        theme
    }
    
    pub fn get_color(&self, name: &str) -> Option<&String> {
        self.colors.get(name)
    }
}

impl Default for Theme {
    fn default() -> Self {
        Self::dark()
    }
}

/// Animation helpers
pub mod animations {
    /// CSS keyframe animation
    pub fn fade_in() -> String {
        r#"
        @keyframes fadeIn {
            from { opacity: 0; }
            to { opacity: 1; }
        }
        .fade-in { animation: fadeIn 0.3s ease-in-out; }
        "#.to_string()
    }
    
    /// Slide in from bottom
    pub fn slide_up() -> String {
        r#"
        @keyframes slideUp {
            from { transform: translateY(20px); opacity: 0; }
            to { transform: translateY(0); opacity: 1; }
        }
        .slide-up { animation: slideUp 0.3s ease-out; }
        "#.to_string()
    }
    
    /// Pulse animation
    pub fn pulse() -> String {
        r#"
        @keyframes pulse {
            0%, 100% { transform: scale(1); }
            50% { transform: scale(1.05); }
        }
        .pulse { animation: pulse 2s infinite; }
        "#.to_string()
    }
    
    /// Spin animation
    pub fn spin() -> String {
        r#"
        @keyframes spin {
            from { transform: rotate(0deg); }
            to { transform: rotate(360deg); }
        }
        .spin { animation: spin 1s linear infinite; }
        "#.to_string()
    }
}

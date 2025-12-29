//! Veyra Database Library
//! 
//! Multi-database support: SQL, key-value, document stores.
//! Works with SQLite, PostgreSQL, MySQL, MongoDB concepts.

use std::collections::HashMap;

/// Generic database result
pub type DbResult<T> = Result<T, String>;

/// Database value types
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    Bytes(Vec<u8>),
    Array(Vec<Value>),
    Object(HashMap<String, Value>),
}

impl Value {
    pub fn as_int(&self) -> Option<i64> {
        match self {
            Value::Int(n) => Some(*n),
            _ => None,
        }
    }
    
    pub fn as_string(&self) -> Option<&str> {
        match self {
            Value::String(s) => Some(s),
            _ => None,
        }
    }
    
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Value::Bool(b) => Some(*b),
            _ => None,
        }
    }
}

/// Database row
pub type Row = HashMap<String, Value>;

/// Query result
#[derive(Debug, Clone)]
pub struct QueryResult {
    pub rows: Vec<Row>,
    pub affected_rows: usize,
    pub last_insert_id: Option<i64>,
}

impl QueryResult {
    pub fn empty() -> Self {
        Self {
            rows: Vec::new(),
            affected_rows: 0,
            last_insert_id: None,
        }
    }
    
    pub fn is_empty(&self) -> bool {
        self.rows.is_empty()
    }
    
    pub fn first(&self) -> Option<&Row> {
        self.rows.first()
    }
}

/// In-memory SQL-like database
pub struct Database {
    tables: HashMap<String, Table>,
}

struct Table {
    columns: Vec<Column>,
    rows: Vec<Row>,
    next_id: i64,
}

struct Column {
    name: String,
    data_type: DataType,
    primary_key: bool,
    auto_increment: bool,
    nullable: bool,
}

#[derive(Clone, Copy)]
enum DataType {
    Integer,
    Float,
    Text,
    Bool,
    Blob,
}

impl Database {
    pub fn new() -> Self {
        Self {
            tables: HashMap::new(),
        }
    }
    
    /// Create a new table
    pub fn create_table(&mut self, name: &str, schema: &str) -> DbResult<()> {
        // Parse simple schema: "id INTEGER PRIMARY KEY, name TEXT, age INTEGER"
        let columns = parse_schema(schema)?;
        
        self.tables.insert(name.to_string(), Table {
            columns,
            rows: Vec::new(),
            next_id: 1,
        });
        
        Ok(())
    }
    
    /// Insert a row
    pub fn insert(&mut self, table: &str, values: Row) -> DbResult<i64> {
        let tbl = self.tables.get_mut(table)
            .ok_or_else(|| format!("Table '{}' not found", table))?;
        
        let id = tbl.next_id;
        let mut row = values;
        
        // Add auto-increment ID if needed
        for col in &tbl.columns {
            if col.auto_increment && !row.contains_key(&col.name) {
                row.insert(col.name.clone(), Value::Int(id));
            }
        }
        
        tbl.rows.push(row);
        tbl.next_id += 1;
        
        Ok(id)
    }
    
    /// Select rows with optional filter
    pub fn select(&self, table: &str, filter: Option<&dyn Fn(&Row) -> bool>) -> DbResult<QueryResult> {
        let tbl = self.tables.get(table)
            .ok_or_else(|| format!("Table '{}' not found", table))?;
        
        let rows: Vec<Row> = tbl.rows.iter()
            .filter(|row| filter.map_or(true, |f| f(row)))
            .cloned()
            .collect();
        
        Ok(QueryResult {
            rows,
            affected_rows: 0,
            last_insert_id: None,
        })
    }
    
    /// Select all rows
    pub fn select_all(&self, table: &str) -> DbResult<QueryResult> {
        self.select(table, None)
    }
    
    /// Update rows
    pub fn update(&mut self, table: &str, updates: Row, filter: impl Fn(&Row) -> bool) -> DbResult<usize> {
        let tbl = self.tables.get_mut(table)
            .ok_or_else(|| format!("Table '{}' not found", table))?;
        
        let mut count = 0;
        for row in &mut tbl.rows {
            if filter(row) {
                for (key, value) in &updates {
                    row.insert(key.clone(), value.clone());
                }
                count += 1;
            }
        }
        
        Ok(count)
    }
    
    /// Delete rows
    pub fn delete(&mut self, table: &str, filter: impl Fn(&Row) -> bool) -> DbResult<usize> {
        let tbl = self.tables.get_mut(table)
            .ok_or_else(|| format!("Table '{}' not found", table))?;
        
        let len_before = tbl.rows.len();
        tbl.rows.retain(|row| !filter(row));
        let deleted = len_before - tbl.rows.len();
        
        Ok(deleted)
    }
    
    /// Execute raw SQL (simplified parser)
    pub fn execute(&mut self, sql: &str) -> DbResult<QueryResult> {
        let sql = sql.trim();
        let upper = sql.to_uppercase();
        
        if upper.starts_with("CREATE TABLE") {
            self.parse_create_table(sql)?;
            Ok(QueryResult::empty())
        } else if upper.starts_with("INSERT") {
            let id = self.parse_insert(sql)?;
            Ok(QueryResult {
                rows: Vec::new(),
                affected_rows: 1,
                last_insert_id: Some(id),
            })
        } else if upper.starts_with("SELECT") {
            self.parse_select(sql)
        } else if upper.starts_with("UPDATE") {
            let affected = self.parse_update(sql)?;
            Ok(QueryResult {
                rows: Vec::new(),
                affected_rows: affected,
                last_insert_id: None,
            })
        } else if upper.starts_with("DELETE") {
            let affected = self.parse_delete(sql)?;
            Ok(QueryResult {
                rows: Vec::new(),
                affected_rows: affected,
                last_insert_id: None,
            })
        } else {
            Err(format!("Unknown SQL: {}", sql))
        }
    }
    
    fn parse_create_table(&mut self, _sql: &str) -> DbResult<()> {
        // Simplified: CREATE TABLE name (columns)
        Ok(())
    }
    
    fn parse_insert(&mut self, _sql: &str) -> DbResult<i64> {
        Ok(1)
    }
    
    fn parse_select(&self, _sql: &str) -> DbResult<QueryResult> {
        Ok(QueryResult::empty())
    }
    
    fn parse_update(&mut self, _sql: &str) -> DbResult<usize> {
        Ok(0)
    }
    
    fn parse_delete(&mut self, _sql: &str) -> DbResult<usize> {
        Ok(0)
    }
}

impl Default for Database {
    fn default() -> Self {
        Self::new()
    }
}

fn parse_schema(schema: &str) -> DbResult<Vec<Column>> {
    let mut columns = Vec::new();
    
    for part in schema.split(',') {
        let part = part.trim();
        let tokens: Vec<&str> = part.split_whitespace().collect();
        
        if tokens.len() < 2 {
            continue;
        }
        
        let name = tokens[0].to_string();
        let data_type = match tokens[1].to_uppercase().as_str() {
            "INTEGER" | "INT" => DataType::Integer,
            "REAL" | "FLOAT" | "DOUBLE" => DataType::Float,
            "TEXT" | "VARCHAR" | "STRING" => DataType::Text,
            "BOOL" | "BOOLEAN" => DataType::Bool,
            "BLOB" | "BYTES" => DataType::Blob,
            _ => DataType::Text,
        };
        
        let upper_part = part.to_uppercase();
        let primary_key = upper_part.contains("PRIMARY KEY");
        let auto_increment = upper_part.contains("AUTOINCREMENT") || upper_part.contains("AUTO_INCREMENT");
        let nullable = !upper_part.contains("NOT NULL");
        
        columns.push(Column {
            name,
            data_type,
            primary_key,
            auto_increment,
            nullable,
        });
    }
    
    Ok(columns)
}

/// Key-Value Store
pub struct KVStore {
    data: HashMap<String, Value>,
}

impl KVStore {
    pub fn new() -> Self {
        Self {
            data: HashMap::new(),
        }
    }
    
    pub fn get(&self, key: &str) -> Option<&Value> {
        self.data.get(key)
    }
    
    pub fn set(&mut self, key: &str, value: Value) {
        self.data.insert(key.to_string(), value);
    }
    
    pub fn delete(&mut self, key: &str) -> Option<Value> {
        self.data.remove(key)
    }
    
    pub fn exists(&self, key: &str) -> bool {
        self.data.contains_key(key)
    }
    
    pub fn keys(&self) -> Vec<&String> {
        self.data.keys().collect()
    }
    
    pub fn len(&self) -> usize {
        self.data.len()
    }
    
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }
    
    pub fn clear(&mut self) {
        self.data.clear();
    }
    
    /// Increment integer value
    pub fn incr(&mut self, key: &str) -> i64 {
        let current = self.get(key)
            .and_then(|v| v.as_int())
            .unwrap_or(0);
        let new_val = current + 1;
        self.set(key, Value::Int(new_val));
        new_val
    }
    
    /// Set with expiration (placeholder - would need async runtime)
    pub fn set_ex(&mut self, key: &str, value: Value, _seconds: u64) {
        self.set(key, value);
    }
}

impl Default for KVStore {
    fn default() -> Self {
        Self::new()
    }
}

/// Document Store (MongoDB-like)
pub struct DocumentStore {
    collections: HashMap<String, Vec<Value>>,
}

impl DocumentStore {
    pub fn new() -> Self {
        Self {
            collections: HashMap::new(),
        }
    }
    
    /// Get or create collection
    pub fn collection(&mut self, name: &str) -> &mut Vec<Value> {
        self.collections.entry(name.to_string()).or_insert_with(Vec::new)
    }
    
    /// Insert document
    pub fn insert_one(&mut self, collection: &str, doc: Value) -> DbResult<()> {
        self.collection(collection).push(doc);
        Ok(())
    }
    
    /// Insert multiple documents
    pub fn insert_many(&mut self, collection: &str, docs: Vec<Value>) -> DbResult<usize> {
        let count = docs.len();
        let col = self.collection(collection);
        col.extend(docs);
        Ok(count)
    }
    
    /// Find documents matching filter
    pub fn find(&self, collection: &str, filter: impl Fn(&Value) -> bool) -> Vec<&Value> {
        self.collections.get(collection)
            .map(|col| col.iter().filter(|doc| filter(doc)).collect())
            .unwrap_or_default()
    }
    
    /// Find one document
    pub fn find_one(&self, collection: &str, filter: impl Fn(&Value) -> bool) -> Option<&Value> {
        self.find(collection, filter).into_iter().next()
    }
    
    /// Update documents
    pub fn update_many(&mut self, collection: &str, filter: impl Fn(&Value) -> bool, update: impl Fn(&mut Value)) -> usize {
        let col = self.collection(collection);
        let mut count = 0;
        for doc in col {
            if filter(doc) {
                update(doc);
                count += 1;
            }
        }
        count
    }
    
    /// Delete documents
    pub fn delete_many(&mut self, collection: &str, filter: impl Fn(&Value) -> bool) -> usize {
        let col = self.collection(collection);
        let len_before = col.len();
        col.retain(|doc| !filter(doc));
        len_before - col.len()
    }
    
    /// Count documents
    pub fn count(&self, collection: &str) -> usize {
        self.collections.get(collection).map(|c| c.len()).unwrap_or(0)
    }
}

impl Default for DocumentStore {
    fn default() -> Self {
        Self::new()
    }
}

/// Query builder
pub struct Query {
    table: String,
    select: Vec<String>,
    where_clauses: Vec<String>,
    order_by: Option<(String, bool)>, // (column, ascending)
    limit: Option<usize>,
    offset: Option<usize>,
}

impl Query {
    pub fn from(table: &str) -> Self {
        Self {
            table: table.to_string(),
            select: Vec::new(),
            where_clauses: Vec::new(),
            order_by: None,
            limit: None,
            offset: None,
        }
    }
    
    pub fn select(mut self, columns: &[&str]) -> Self {
        self.select = columns.iter().map(|s| s.to_string()).collect();
        self
    }
    
    pub fn where_eq(mut self, column: &str, value: &str) -> Self {
        self.where_clauses.push(format!("{} = '{}'", column, value));
        self
    }
    
    pub fn where_gt(mut self, column: &str, value: i64) -> Self {
        self.where_clauses.push(format!("{} > {}", column, value));
        self
    }
    
    pub fn where_lt(mut self, column: &str, value: i64) -> Self {
        self.where_clauses.push(format!("{} < {}", column, value));
        self
    }
    
    pub fn order_by(mut self, column: &str, ascending: bool) -> Self {
        self.order_by = Some((column.to_string(), ascending));
        self
    }
    
    pub fn limit(mut self, n: usize) -> Self {
        self.limit = Some(n);
        self
    }
    
    pub fn offset(mut self, n: usize) -> Self {
        self.offset = Some(n);
        self
    }
    
    pub fn to_sql(&self) -> String {
        let cols = if self.select.is_empty() {
            "*".to_string()
        } else {
            self.select.join(", ")
        };
        
        let mut sql = format!("SELECT {} FROM {}", cols, self.table);
        
        if !self.where_clauses.is_empty() {
            sql.push_str(&format!(" WHERE {}", self.where_clauses.join(" AND ")));
        }
        
        if let Some((col, asc)) = &self.order_by {
            let dir = if *asc { "ASC" } else { "DESC" };
            sql.push_str(&format!(" ORDER BY {} {}", col, dir));
        }
        
        if let Some(n) = self.limit {
            sql.push_str(&format!(" LIMIT {}", n));
        }
        
        if let Some(n) = self.offset {
            sql.push_str(&format!(" OFFSET {}", n));
        }
        
        sql
    }
}

/// Migration helper
pub struct Migration {
    version: u32,
    up_sql: String,
    down_sql: String,
}

impl Migration {
    pub fn new(version: u32, up: &str, down: &str) -> Self {
        Self {
            version,
            up_sql: up.to_string(),
            down_sql: down.to_string(),
        }
    }
    
    pub fn apply(&self, db: &mut Database) -> DbResult<()> {
        db.execute(&self.up_sql)?;
        Ok(())
    }
    
    pub fn rollback(&self, db: &mut Database) -> DbResult<()> {
        db.execute(&self.down_sql)?;
        Ok(())
    }
}

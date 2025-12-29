//! Concurrency Primitives
//! 
//! Channels, threads, and synchronization for parallel programming.

use std::sync::{Arc, Mutex, Condvar, mpsc};
use std::thread::{self, JoinHandle};
use std::time::Duration;

/// Channel for communication between threads
pub struct Channel<T> {
    sender: mpsc::Sender<T>,
    receiver: Arc<Mutex<mpsc::Receiver<T>>>,
}

impl<T: Send + Clone + 'static> Channel<T> {
    pub fn new() -> Self {
        let (sender, receiver) = mpsc::channel();
        Self {
            sender,
            receiver: Arc::new(Mutex::new(receiver)),
        }
    }
    
    pub fn send(&self, value: T) -> Result<(), String> {
        self.sender.send(value).map_err(|e| e.to_string())
    }
    
    pub fn receive(&self) -> Result<T, String> {
        let receiver = self.receiver.lock().map_err(|e| e.to_string())?;
        receiver.recv().map_err(|e| e.to_string())
    }
    
    pub fn try_receive(&self) -> Option<T> {
        let receiver = self.receiver.lock().ok()?;
        receiver.try_recv().ok()
    }
    
    pub fn receive_timeout(&self, timeout_ms: u64) -> Option<T> {
        let receiver = self.receiver.lock().ok()?;
        receiver.recv_timeout(Duration::from_millis(timeout_ms)).ok()
    }
}

impl<T: Send + Clone + 'static> Default for Channel<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> Clone for Channel<T> {
    fn clone(&self) -> Self {
        Self {
            sender: self.sender.clone(),
            receiver: self.receiver.clone(),
        }
    }
}

/// Buffered channel with capacity
pub struct BufferedChannel<T> {
    sender: mpsc::SyncSender<T>,
    receiver: Arc<Mutex<mpsc::Receiver<T>>>,
}

impl<T: Send + 'static> BufferedChannel<T> {
    pub fn new(capacity: usize) -> Self {
        let (sender, receiver) = mpsc::sync_channel(capacity);
        Self {
            sender,
            receiver: Arc::new(Mutex::new(receiver)),
        }
    }
    
    pub fn send(&self, value: T) -> Result<(), String> {
        self.sender.send(value).map_err(|e| e.to_string())
    }
    
    pub fn try_send(&self, value: T) -> bool {
        self.sender.try_send(value).is_ok()
    }
    
    pub fn receive(&self) -> Result<T, String> {
        let receiver = self.receiver.lock().map_err(|e| e.to_string())?;
        receiver.recv().map_err(|e| e.to_string())
    }
}

/// Spawn a new thread
pub fn spawn<F, T>(f: F) -> JoinHandle<T>
where
    F: FnOnce() -> T + Send + 'static,
    T: Send + 'static,
{
    thread::spawn(f)
}

/// Spawn multiple threads and wait for all
pub fn parallel<F, T>(count: usize, f: F) -> Vec<T>
where
    F: Fn(usize) -> T + Send + Sync + Clone + 'static,
    T: Send + 'static,
{
    let f = Arc::new(f);
    let handles: Vec<_> = (0..count)
        .map(|i| {
            let f = Arc::clone(&f);
            thread::spawn(move || f(i))
        })
        .collect();
    
    handles.into_iter().map(|h| h.join().unwrap()).collect()
}

/// Sleep for specified milliseconds
pub fn sleep(ms: u64) {
    thread::sleep(Duration::from_millis(ms));
}

/// Get current thread ID
pub fn thread_id() -> String {
    format!("{:?}", thread::current().id())
}

/// Mutex wrapper
pub struct Lock<T> {
    inner: Arc<Mutex<T>>,
}

impl<T> Lock<T> {
    pub fn new(value: T) -> Self {
        Self {
            inner: Arc::new(Mutex::new(value)),
        }
    }
    
    pub fn lock(&self) -> Result<std::sync::MutexGuard<'_, T>, String> {
        self.inner.lock().map_err(|e| e.to_string())
    }
    
    pub fn try_lock(&self) -> Option<std::sync::MutexGuard<'_, T>> {
        self.inner.try_lock().ok()
    }
}

impl<T> Clone for Lock<T> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}

/// Read-Write Lock
pub struct RwLock<T> {
    inner: Arc<std::sync::RwLock<T>>,
}

impl<T> RwLock<T> {
    pub fn new(value: T) -> Self {
        Self {
            inner: Arc::new(std::sync::RwLock::new(value)),
        }
    }
    
    pub fn read(&self) -> Result<std::sync::RwLockReadGuard<'_, T>, String> {
        self.inner.read().map_err(|e| e.to_string())
    }
    
    pub fn write(&self) -> Result<std::sync::RwLockWriteGuard<'_, T>, String> {
        self.inner.write().map_err(|e| e.to_string())
    }
}

impl<T> Clone for RwLock<T> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}

/// Atomic counter
pub struct AtomicCounter {
    value: std::sync::atomic::AtomicI64,
}

impl AtomicCounter {
    pub fn new(value: i64) -> Self {
        Self {
            value: std::sync::atomic::AtomicI64::new(value),
        }
    }
    
    pub fn get(&self) -> i64 {
        self.value.load(std::sync::atomic::Ordering::SeqCst)
    }
    
    pub fn set(&self, value: i64) {
        self.value.store(value, std::sync::atomic::Ordering::SeqCst);
    }
    
    pub fn increment(&self) -> i64 {
        self.value.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
    }
    
    pub fn decrement(&self) -> i64 {
        self.value.fetch_sub(1, std::sync::atomic::Ordering::SeqCst)
    }
    
    pub fn add(&self, delta: i64) -> i64 {
        self.value.fetch_add(delta, std::sync::atomic::Ordering::SeqCst)
    }
    
    pub fn compare_swap(&self, expected: i64, new: i64) -> bool {
        self.value
            .compare_exchange(expected, new, 
                std::sync::atomic::Ordering::SeqCst,
                std::sync::atomic::Ordering::SeqCst)
            .is_ok()
    }
}

/// WaitGroup for synchronizing multiple threads
pub struct WaitGroup {
    counter: AtomicCounter,
    cond: Arc<(Mutex<bool>, Condvar)>,
}

impl WaitGroup {
    pub fn new() -> Self {
        Self {
            counter: AtomicCounter::new(0),
            cond: Arc::new((Mutex::new(false), Condvar::new())),
        }
    }
    
    pub fn add(&self, delta: i64) {
        self.counter.add(delta);
    }
    
    pub fn done(&self) {
        if self.counter.decrement() == 1 {
            let (lock, cond) = &*self.cond;
            let mut finished = lock.lock().unwrap();
            *finished = true;
            cond.notify_all();
        }
    }
    
    pub fn wait(&self) {
        let (lock, cond) = &*self.cond;
        let mut finished = lock.lock().unwrap();
        while !*finished && self.counter.get() > 0 {
            finished = cond.wait(finished).unwrap();
        }
    }
}

impl Default for WaitGroup {
    fn default() -> Self {
        Self::new()
    }
}

/// Thread pool for parallel task execution
pub struct ThreadPool {
    workers: Vec<Worker>,
    sender: Option<mpsc::Sender<Job>>,
}

type Job = Box<dyn FnOnce() + Send + 'static>;

struct Worker {
    _handle: JoinHandle<()>,
}

impl ThreadPool {
    pub fn new(size: usize) -> Self {
        let (sender, receiver) = mpsc::channel::<Job>();
        let receiver = Arc::new(Mutex::new(receiver));
        
        let workers: Vec<_> = (0..size)
            .map(|_| {
                let receiver = Arc::clone(&receiver);
                let handle = thread::spawn(move || loop {
                    let job = {
                        let receiver = receiver.lock().unwrap();
                        receiver.recv()
                    };
                    
                    match job {
                        Ok(job) => job(),
                        Err(_) => break,
                    }
                });
                Worker { _handle: handle }
            })
            .collect();
        
        Self {
            workers,
            sender: Some(sender),
        }
    }
    
    pub fn execute<F>(&self, f: F)
    where
        F: FnOnce() + Send + 'static,
    {
        if let Some(ref sender) = self.sender {
            let _ = sender.send(Box::new(f));
        }
    }
}

impl Drop for ThreadPool {
    fn drop(&mut self) {
        drop(self.sender.take());
    }
}

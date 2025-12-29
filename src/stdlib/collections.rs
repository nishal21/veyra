//! Collection Operations
//! 
//! Array, list, and map functions.

use std::collections::HashMap;

/// Create a new vector
pub fn vec_new<T>() -> Vec<T> {
    Vec::new()
}

/// Create vector with capacity
pub fn vec_with_capacity<T>(capacity: usize) -> Vec<T> {
    Vec::with_capacity(capacity)
}

/// Push element to vector
pub fn push<T>(vec: &mut Vec<T>, item: T) {
    vec.push(item);
}

/// Pop element from vector
pub fn pop<T>(vec: &mut Vec<T>) -> Option<T> {
    vec.pop()
}

/// Get first element
pub fn first<T: Clone>(vec: &[T]) -> Option<T> {
    vec.first().cloned()
}

/// Get last element
pub fn last<T: Clone>(vec: &[T]) -> Option<T> {
    vec.last().cloned()
}

/// Get element at index
pub fn get<T: Clone>(vec: &[T], index: usize) -> Option<T> {
    vec.get(index).cloned()
}

/// Set element at index
pub fn set<T>(vec: &mut [T], index: usize, value: T) {
    if index < vec.len() {
        vec[index] = value;
    }
}

/// Insert at index
pub fn insert<T>(vec: &mut Vec<T>, index: usize, value: T) {
    vec.insert(index, value);
}

/// Remove at index
pub fn remove<T>(vec: &mut Vec<T>, index: usize) -> T {
    vec.remove(index)
}

/// Get vector length
pub fn len<T>(vec: &[T]) -> usize {
    vec.len()
}

/// Check if vector is empty
pub fn is_empty<T>(vec: &[T]) -> bool {
    vec.is_empty()
}

/// Clear vector
pub fn clear<T>(vec: &mut Vec<T>) {
    vec.clear();
}

/// Reverse vector in place
pub fn reverse<T>(vec: &mut [T]) {
    vec.reverse();
}

/// Sort vector
pub fn sort<T: Ord>(vec: &mut [T]) {
    vec.sort();
}

/// Find index of element
pub fn index_of<T: PartialEq>(vec: &[T], item: &T) -> Option<usize> {
    vec.iter().position(|x| x == item)
}

/// Check if vector contains element
pub fn vec_contains<T: PartialEq>(vec: &[T], item: &T) -> bool {
    vec.contains(item)
}

/// Slice of vector
pub fn slice<T: Clone>(vec: &[T], start: usize, end: usize) -> Vec<T> {
    vec[start..end.min(vec.len())].to_vec()
}

/// Concatenate two vectors
pub fn concat<T: Clone>(a: &[T], b: &[T]) -> Vec<T> {
    let mut result = a.to_vec();
    result.extend(b.iter().cloned());
    result
}

/// Map function over vector
pub fn map<T, U, F>(vec: Vec<T>, f: F) -> Vec<U>
where
    F: Fn(T) -> U,
{
    vec.into_iter().map(f).collect()
}

/// Filter vector by predicate
pub fn filter<T, F>(vec: Vec<T>, f: F) -> Vec<T>
where
    F: Fn(&T) -> bool,
{
    vec.into_iter().filter(f).collect()
}

/// Reduce vector to single value
pub fn reduce<T, U, F>(vec: Vec<T>, init: U, f: F) -> U
where
    F: Fn(U, T) -> U,
{
    vec.into_iter().fold(init, f)
}

/// Find first element matching predicate
pub fn find<T, F>(vec: &[T], f: F) -> Option<&T>
where
    F: Fn(&T) -> bool,
{
    vec.iter().find(|item| f(item))
}

/// Check if any element matches predicate
pub fn any<T, F>(vec: &[T], f: F) -> bool
where
    F: Fn(&T) -> bool,
{
    vec.iter().any(f)
}

/// Check if all elements match predicate
pub fn all<T, F>(vec: &[T], f: F) -> bool
where
    F: Fn(&T) -> bool,
{
    vec.iter().all(f)
}

/// Create range as vector
pub fn range(start: i64, end: i64) -> Vec<i64> {
    (start..end).collect()
}

/// Create range with step
pub fn range_step(start: i64, end: i64, step: i64) -> Vec<i64> {
    let mut result = Vec::new();
    let mut i = start;
    while i < end {
        result.push(i);
        i += step;
    }
    result
}

/// Zip two vectors
pub fn zip<A, B>(a: Vec<A>, b: Vec<B>) -> Vec<(A, B)> {
    a.into_iter().zip(b.into_iter()).collect()
}

/// Enumerate vector
pub fn enumerate<T>(vec: Vec<T>) -> Vec<(usize, T)> {
    vec.into_iter().enumerate().collect()
}

/// Flatten nested vectors
pub fn flatten<T>(nested: Vec<Vec<T>>) -> Vec<T> {
    nested.into_iter().flatten().collect()
}

/// Sum of numeric vector
pub fn sum_int(vec: &[i64]) -> i64 {
    vec.iter().sum()
}

/// Sum of float vector
pub fn sum_float(vec: &[f64]) -> f64 {
    vec.iter().sum()
}

/// Average of float vector
pub fn average(vec: &[f64]) -> f64 {
    if vec.is_empty() {
        0.0
    } else {
        vec.iter().sum::<f64>() / vec.len() as f64
    }
}

/// Create new hashmap
pub fn map_new<K, V>() -> HashMap<K, V> {
    HashMap::new()
}

/// Insert into hashmap
pub fn map_insert<K: std::hash::Hash + Eq, V>(map: &mut HashMap<K, V>, key: K, value: V) {
    map.insert(key, value);
}

/// Get from hashmap
pub fn map_get<K: std::hash::Hash + Eq, V: Clone>(map: &HashMap<K, V>, key: &K) -> Option<V> {
    map.get(key).cloned()
}

/// Remove from hashmap
pub fn map_remove<K: std::hash::Hash + Eq, V>(map: &mut HashMap<K, V>, key: &K) -> Option<V> {
    map.remove(key)
}

/// Check if hashmap contains key
pub fn map_contains<K: std::hash::Hash + Eq, V>(map: &HashMap<K, V>, key: &K) -> bool {
    map.contains_key(key)
}

/// Get hashmap keys
pub fn map_keys<K: Clone, V>(map: &HashMap<K, V>) -> Vec<K> {
    map.keys().cloned().collect()
}

/// Get hashmap values
pub fn map_values<K, V: Clone>(map: &HashMap<K, V>) -> Vec<V> {
    map.values().cloned().collect()
}

use ::rand::seq::SliceRandom;
use ::rand::{thread_rng, Rng};
use pyo3::exceptions::{PyIndexError, PyRuntimeError, PyValueError};
use pyo3::prelude::*;
use pyo3::types::{PyAny, PyDict, PyList, PyModule, PyTuple};
use std::collections::HashMap;
use std::path::PathBuf;
use std::time::{SystemTime, UNIX_EPOCH};

#[pyclass(name = "Some")]
struct LangSome {
    value: Py<PyAny>,
}

#[pymethods]
impl LangSome {
    #[new]
    fn new(value: Py<PyAny>) -> Self {
        Self { value }
    }

    fn unwrap(&self, py: Python<'_>) -> Py<PyAny> {
        self.value.clone_ref(py)
    }

    fn expect(&self, py: Python<'_>, _message: &str) -> Py<PyAny> {
        self.value.clone_ref(py)
    }

    fn unwrap_or(&self, py: Python<'_>, _default: Py<PyAny>) -> Py<PyAny> {
        self.value.clone_ref(py)
    }

    fn map(&self, py: Python<'_>, func: Py<PyAny>) -> PyResult<LangSome> {
        Ok(LangSome::new(func.call1(py, (self.value.clone_ref(py),))?))
    }

    fn is_some(&self) -> bool {
        true
    }

    fn is_nothing(&self) -> bool {
        false
    }

    fn __bool__(&self) -> bool {
        true
    }

    fn __repr__(&self, py: Python<'_>) -> PyResult<String> {
        Ok(format!("Some({})", self.value.bind(py).repr()?))
    }

    fn __str__(&self, py: Python<'_>) -> PyResult<String> {
        Ok(format!("Some({})", self.value.bind(py).str()?))
    }
}

#[pyclass(name = "Nothing")]
#[derive(Clone, Copy)]
struct LangNothing;

#[pymethods]
impl LangNothing {
    #[new]
    fn new() -> Self {
        Self
    }

    fn unwrap(&self) -> PyResult<()> {
        Err(PyValueError::new_err("Cannot unwrap Nothing"))
    }

    fn expect(&self, message: &str) -> PyResult<()> {
        Err(PyValueError::new_err(message.to_string()))
    }

    fn unwrap_or(&self, py: Python<'_>, default: Py<PyAny>) -> Py<PyAny> {
        default.clone_ref(py)
    }

    fn map(&self) -> Self {
        *self
    }

    fn is_some(&self) -> bool {
        false
    }

    fn is_nothing(&self) -> bool {
        true
    }

    fn __bool__(&self) -> bool {
        false
    }

    fn __repr__(&self) -> &'static str {
        "Nothing"
    }

    fn __str__(&self) -> &'static str {
        "Nothing"
    }
}

#[pyclass(name = "Ok")]
struct LangOk {
    value: Py<PyAny>,
}

#[pymethods]
impl LangOk {
    #[new]
    fn new(value: Py<PyAny>) -> Self {
        Self { value }
    }

    fn unwrap(&self, py: Python<'_>) -> Py<PyAny> {
        self.value.clone_ref(py)
    }

    fn expect(&self, py: Python<'_>, _message: &str) -> Py<PyAny> {
        self.value.clone_ref(py)
    }

    fn unwrap_or(&self, py: Python<'_>, _default: Py<PyAny>) -> Py<PyAny> {
        self.value.clone_ref(py)
    }

    fn map(&self, py: Python<'_>, func: Py<PyAny>) -> PyResult<LangOk> {
        Ok(LangOk::new(func.call1(py, (self.value.clone_ref(py),))?))
    }

    fn map_err(&self, py: Python<'_>, _func: Py<PyAny>) -> LangOk {
        LangOk::new(self.value.clone_ref(py))
    }

    fn is_ok(&self) -> bool {
        true
    }

    fn is_err(&self) -> bool {
        false
    }

    fn __bool__(&self) -> bool {
        true
    }

    fn __repr__(&self, py: Python<'_>) -> PyResult<String> {
        Ok(format!("Ok({})", self.value.bind(py).repr()?))
    }

    fn __str__(&self, py: Python<'_>) -> PyResult<String> {
        Ok(format!("Ok({})", self.value.bind(py).str()?))
    }
}

#[pyclass(name = "Err")]
struct LangErr {
    error: Py<PyAny>,
}

#[pymethods]
impl LangErr {
    #[new]
    fn new(error: Py<PyAny>) -> Self {
        Self { error }
    }

    fn unwrap(&self, py: Python<'_>) -> PyResult<Py<PyAny>> {
        Err(PyRuntimeError::new_err(
            self.error.bind(py).str()?.to_string(),
        ))
    }

    fn expect(&self, _py: Python<'_>, message: &str) -> PyResult<Py<PyAny>> {
        Err(PyRuntimeError::new_err(message.to_string()))
    }

    fn unwrap_or(&self, py: Python<'_>, default: Py<PyAny>) -> Py<PyAny> {
        default.clone_ref(py)
    }

    fn map(&self, py: Python<'_>, _func: Py<PyAny>) -> LangErr {
        LangErr {
            error: self.error.clone_ref(py),
        }
    }

    fn map_err(&self, py: Python<'_>, func: Py<PyAny>) -> PyResult<LangErr> {
        Ok(LangErr::new(func.call1(py, (self.error.clone_ref(py),))?))
    }

    fn is_ok(&self) -> bool {
        false
    }

    fn is_err(&self) -> bool {
        true
    }

    fn __bool__(&self) -> bool {
        false
    }

    fn __repr__(&self, py: Python<'_>) -> PyResult<String> {
        Ok(format!("Err({})", self.error.bind(py).repr()?))
    }

    fn __str__(&self, py: Python<'_>) -> PyResult<String> {
        Ok(format!("Err({})", self.error.bind(py).str()?))
    }
}

#[pyclass(name = "Iterator")]
struct LangIterator {
    items: Py<PyList>,
}

#[pymethods]
impl LangIterator {
    #[new]
    fn new(py: Python<'_>, iterable: &Bound<'_, PyAny>) -> PyResult<Self> {
        Ok(Self {
            items: iterable_to_list(py, iterable)?.unbind(),
        })
    }

    fn to_list(&self, py: Python<'_>) -> PyResult<Py<PyList>> {
        Ok(PyList::new_bound(py, self.items.bind(py).iter()).unbind())
    }

    fn to_tuple(&self, py: Python<'_>) -> PyResult<Py<PyTuple>> {
        Ok(PyTuple::new_bound(py, self.items.bind(py).iter()).unbind())
    }

    fn map(&self, py: Python<'_>, func: Py<PyAny>) -> PyResult<Self> {
        let out = PyList::empty_bound(py);
        for item in self.items.bind(py).iter() {
            out.append(func.call1(py, (item,))?)?;
        }
        Ok(Self {
            items: out.unbind(),
        })
    }

    fn filter(&self, py: Python<'_>, func: Py<PyAny>) -> PyResult<Self> {
        let out = PyList::empty_bound(py);
        for item in self.items.bind(py).iter() {
            if func.call1(py, (item.clone(),))?.is_truthy(py)? {
                out.append(item)?;
            }
        }
        Ok(Self {
            items: out.unbind(),
        })
    }

    fn fold(&self, py: Python<'_>, initial: Py<PyAny>, func: Py<PyAny>) -> PyResult<Py<PyAny>> {
        let mut acc = initial;
        for item in self.items.bind(py).iter() {
            acc = func.call1(py, (acc, item))?;
        }
        Ok(acc)
    }

    fn collect(&self, py: Python<'_>) -> PyResult<Py<PyList>> {
        self.to_list(py)
    }

    #[pyo3(signature = (func=None))]
    fn any(&self, py: Python<'_>, func: Option<Py<PyAny>>) -> PyResult<bool> {
        for item in self.items.bind(py).iter() {
            let ok = match &func {
                Some(func) => func.call1(py, (item,))?.is_truthy(py)?,
                None => item.is_truthy()?,
            };
            if ok {
                return Ok(true);
            }
        }
        Ok(false)
    }

    #[pyo3(signature = (func=None))]
    fn all(&self, py: Python<'_>, func: Option<Py<PyAny>>) -> PyResult<bool> {
        for item in self.items.bind(py).iter() {
            let ok = match &func {
                Some(func) => func.call1(py, (item,))?.is_truthy(py)?,
                None => item.is_truthy()?,
            };
            if !ok {
                return Ok(false);
            }
        }
        Ok(true)
    }

    fn __len__(&self, py: Python<'_>) -> usize {
        self.items.bind(py).len()
    }

    fn __iter__(&self, py: Python<'_>) -> PyResult<Py<PyAny>> {
        let builtins = PyModule::import_bound(py, "builtins")?;
        Ok(builtins
            .getattr("iter")?
            .call1((self.items.bind(py),))?
            .unbind())
    }

    fn __getitem__(&self, py: Python<'_>, index: isize) -> PyResult<Py<PyAny>> {
        let list = self.items.bind(py);
        let len = list.len() as isize;
        let index = if index < 0 { len + index } else { index };
        if index < 0 || index >= len {
            return Err(PyIndexError::new_err("Iterator index out of range"));
        }
        Ok(list.get_item(index as usize)?.unbind())
    }

    fn __repr__(&self, py: Python<'_>) -> PyResult<String> {
        Ok(format!("Iterator({})", self.items.bind(py).repr()?))
    }

    fn __str__(&self, py: Python<'_>) -> PyResult<String> {
        self.__repr__(py)
    }
}

#[pyclass(name = "Path")]
#[derive(Clone)]
struct LangPath {
    raw: String,
}

#[pymethods]
impl LangPath {
    #[new]
    fn new(raw: &str) -> Self {
        Self {
            raw: raw.to_string(),
        }
    }

    fn join(&self, parts: Vec<String>) -> Self {
        let mut path = PathBuf::from(&self.raw);
        for part in parts {
            path.push(part);
        }
        Self {
            raw: path.to_string_lossy().to_string(),
        }
    }

    fn parent(&self) -> LangNothingOrSomeString {
        match PathBuf::from(&self.raw).parent() {
            Some(parent) => LangNothingOrSomeString::Some(parent.to_string_lossy().to_string()),
            None => LangNothingOrSomeString::Nothing,
        }
    }

    fn __str__(&self) -> String {
        self.raw.clone()
    }

    fn __repr__(&self) -> String {
        format!("Path({:?})", self.raw)
    }
}

enum LangNothingOrSomeString {
    Some(String),
    Nothing,
}

impl IntoPy<PyObject> for LangNothingOrSomeString {
    fn into_py(self, py: Python<'_>) -> PyObject {
        match self {
            LangNothingOrSomeString::Some(value) => LangSome::new(value.into_py(py)).into_py(py),
            LangNothingOrSomeString::Nothing => LangNothing.into_py(py),
        }
    }
}

#[pyfunction(name = "len")]
fn lang_len(obj: &Bound<'_, PyAny>) -> PyResult<usize> {
    obj.len()
}

#[pyfunction(name = "iter")]
fn lang_iter(py: Python<'_>, obj: &Bound<'_, PyAny>) -> PyResult<LangIterator> {
    LangIterator::new(py, obj)
}

#[pyfunction(name = "range")]
fn lang_range(py: Python<'_>, start: i64, end: i64) -> PyResult<LangIterator> {
    let values = if start <= end {
        (start..end)
            .map(|value| value.into_py(py))
            .collect::<Vec<_>>()
    } else {
        (end + 1..=start)
            .rev()
            .map(|value| value.into_py(py))
            .collect::<Vec<_>>()
    };
    Ok(LangIterator {
        items: PyList::new_bound(py, values).unbind(),
    })
}

#[pyfunction]
fn enumerate(py: Python<'_>, obj: &Bound<'_, PyAny>) -> PyResult<LangIterator> {
    let out = PyList::empty_bound(py);
    for (index, item) in iterable_to_list(py, obj)?.iter().enumerate() {
        out.append((index, item))?;
    }
    Ok(LangIterator {
        items: out.unbind(),
    })
}

#[pyfunction]
fn zip(
    py: Python<'_>,
    left: &Bound<'_, PyAny>,
    right: &Bound<'_, PyAny>,
) -> PyResult<LangIterator> {
    let left = iterable_to_list(py, left)?;
    let right = iterable_to_list(py, right)?;
    let out = PyList::empty_bound(py);
    let count = left.len().min(right.len());
    for index in 0..count {
        out.append((left.get_item(index)?, right.get_item(index)?))?;
    }
    Ok(LangIterator {
        items: out.unbind(),
    })
}

#[pyfunction]
fn map(py: Python<'_>, obj: &Bound<'_, PyAny>, func: Py<PyAny>) -> PyResult<LangIterator> {
    LangIterator::new(py, obj)?.map(py, func)
}

#[pyfunction]
fn filter(py: Python<'_>, obj: &Bound<'_, PyAny>, func: Py<PyAny>) -> PyResult<LangIterator> {
    LangIterator::new(py, obj)?.filter(py, func)
}

#[pyfunction]
fn fold(
    py: Python<'_>,
    obj: &Bound<'_, PyAny>,
    initial: Py<PyAny>,
    func: Py<PyAny>,
) -> PyResult<Py<PyAny>> {
    LangIterator::new(py, obj)?.fold(py, initial, func)
}

#[pyfunction]
fn collect(py: Python<'_>, obj: &Bound<'_, PyAny>) -> PyResult<Py<PyList>> {
    LangIterator::new(py, obj)?.collect(py)
}

#[pyfunction]
#[pyo3(signature = (obj, func=None))]
fn any(py: Python<'_>, obj: &Bound<'_, PyAny>, func: Option<Py<PyAny>>) -> PyResult<bool> {
    LangIterator::new(py, obj)?.any(py, func)
}

#[pyfunction]
#[pyo3(signature = (obj, func=None))]
fn all(py: Python<'_>, obj: &Bound<'_, PyAny>, func: Option<Py<PyAny>>) -> PyResult<bool> {
    LangIterator::new(py, obj)?.all(py, func)
}

#[pyfunction]
fn panic(message: &str) -> PyResult<()> {
    Err(PyRuntimeError::new_err(message.to_string()))
}

#[pyfunction]
fn type_name(obj: &Bound<'_, PyAny>) -> PyResult<String> {
    Ok(obj.get_type().name()?.to_string())
}

#[pyfunction]
fn debug(obj: &Bound<'_, PyAny>) -> PyResult<String> {
    Ok(obj.repr()?.to_string())
}

#[pyfunction]
fn upper(value: &str) -> String {
    value.to_uppercase()
}

#[pyfunction]
fn lower(value: &str) -> String {
    value.to_lowercase()
}

#[pyfunction]
fn title(value: &str) -> String {
    value
        .split_whitespace()
        .map(|word| {
            let mut chars = word.chars();
            match chars.next() {
                Some(first) => format!("{}{}", first.to_uppercase(), chars.as_str().to_lowercase()),
                None => String::new(),
            }
        })
        .collect::<Vec<_>>()
        .join(" ")
}

#[pyfunction]
fn strip(value: &str) -> String {
    value.trim().to_string()
}

#[pyfunction]
#[pyo3(signature = (value, sep=None))]
fn split(value: &str, sep: Option<&str>) -> Vec<String> {
    match sep {
        Some(sep) => value.split(sep).map(str::to_string).collect(),
        None => value.split_whitespace().map(str::to_string).collect(),
    }
}

#[pyfunction]
fn join(sep: &str, values: Vec<String>) -> String {
    values.join(sep)
}

#[pyfunction]
fn replace(value: &str, from: &str, to: &str) -> String {
    value.replace(from, to)
}

#[pyfunction]
fn contains(value: &str, needle: &str) -> bool {
    value.contains(needle)
}

#[pyfunction]
fn starts_with(value: &str, prefix: &str) -> bool {
    value.starts_with(prefix)
}

#[pyfunction]
fn ends_with(value: &str, suffix: &str) -> bool {
    value.ends_with(suffix)
}

#[pyfunction(name = "Vec")]
#[pyo3(signature = (obj=None))]
fn vec_ctor(py: Python<'_>, obj: Option<&Bound<'_, PyAny>>) -> PyResult<Py<PyList>> {
    match obj {
        Some(obj) => Ok(iterable_to_list(py, obj)?.unbind()),
        None => Ok(PyList::empty_bound(py).unbind()),
    }
}

#[pyfunction(name = "Map")]
#[pyo3(signature = (obj=None))]
fn map_ctor(py: Python<'_>, obj: Option<&Bound<'_, PyAny>>) -> PyResult<Py<PyDict>> {
    let dict = PyDict::new_bound(py);
    if let Some(obj) = obj {
        for item in obj.iter()? {
            let item = item?;
            let tuple = item.downcast::<PyTuple>()?;
            if tuple.len() != 2 {
                return Err(PyValueError::new_err("Map entries must be pairs"));
            }
            dict.set_item(tuple.get_item(0)?, tuple.get_item(1)?)?;
        }
    }
    Ok(dict.unbind())
}

#[pyfunction(name = "Set")]
#[pyo3(signature = (obj=None))]
fn set_ctor(py: Python<'_>, obj: Option<&Bound<'_, PyAny>>) -> PyResult<PyObject> {
    let builtins = PyModule::import_bound(py, "builtins")?;
    match obj {
        Some(obj) => builtins
            .getattr("set")?
            .call1((obj,))
            .map(|set| set.unbind()),
        None => builtins.getattr("set")?.call0().map(|set| set.unbind()),
    }
}

#[pyfunction(name = "Counter")]
fn counter_ctor(py: Python<'_>, obj: &Bound<'_, PyAny>) -> PyResult<Py<PyDict>> {
    let dict = PyDict::new_bound(py);
    for item in obj.iter()? {
        let item = item?;
        let current = dict
            .get_item(&item)?
            .and_then(|value| value.extract::<usize>().ok())
            .unwrap_or(0);
        dict.set_item(&item, current + 1)?;
    }
    Ok(dict.unbind())
}

#[pyfunction(name = "min")]
fn lang_min(left: f64, right: f64) -> f64 {
    left.min(right)
}

#[pyfunction(name = "max")]
fn lang_max(left: f64, right: f64) -> f64 {
    left.max(right)
}

#[pyfunction(name = "abs")]
fn lang_abs(value: f64) -> f64 {
    value.abs()
}

#[pyfunction(name = "round")]
fn lang_round(value: f64) -> f64 {
    value.round()
}

#[pyfunction]
fn floor(value: f64) -> f64 {
    value.floor()
}

#[pyfunction]
fn ceil(value: f64) -> f64 {
    value.ceil()
}

#[pyfunction]
fn sqrt(value: f64) -> PyResult<f64> {
    if value < 0.0 {
        return Err(PyValueError::new_err("sqrt expected a non-negative number"));
    }
    Ok(value.sqrt())
}

#[pyfunction]
fn clamp(value: f64, low: f64, high: f64) -> f64 {
    value.max(low).min(high)
}

#[pyfunction(name = "rand")]
fn rand_fn() -> f64 {
    thread_rng().gen()
}

#[pyfunction]
fn randint(low: i64, high: i64) -> PyResult<i64> {
    if low > high {
        return Err(PyValueError::new_err("randint low must be <= high"));
    }
    Ok(thread_rng().gen_range(low..=high))
}

#[pyfunction]
fn choice(py: Python<'_>, obj: &Bound<'_, PyAny>) -> PyResult<PyObject> {
    let values = iterable_to_list(py, obj)?;
    if values.is_empty() {
        return Ok(
            LangErr::new("choice() cannot choose from an empty sequence".into_py(py)).into_py(py),
        );
    }
    let index = thread_rng().gen_range(0..values.len());
    Ok(LangOk::new(values.get_item(index)?.unbind()).into_py(py))
}

#[pyfunction]
fn shuffle(py: Python<'_>, obj: &Bound<'_, PyAny>) -> PyResult<Py<PyList>> {
    let values = iterable_to_list(py, obj)?;
    let mut objects = values.iter().map(|item| item.unbind()).collect::<Vec<_>>();
    objects.shuffle(&mut thread_rng());
    Ok(PyList::new_bound(py, objects).unbind())
}

#[pyfunction]
fn cwd() -> LangPath {
    LangPath {
        raw: std::env::current_dir()
            .unwrap_or_else(|_| PathBuf::from("."))
            .to_string_lossy()
            .to_string(),
    }
}

#[pyfunction]
fn home() -> LangNothingOrSomeString {
    let home = std::env::var("USERPROFILE")
        .ok()
        .or_else(|| std::env::var("HOME").ok());
    match home {
        Some(path) => LangNothingOrSomeString::Some(path),
        None => LangNothingOrSomeString::Nothing,
    }
}

#[pyfunction]
fn read_text(py: Python<'_>, path: &str) -> PyObject {
    match std::fs::read_to_string(path) {
        Ok(value) => LangOk::new(value.into_py(py)).into_py(py),
        Err(error) => LangErr::new(error.to_string().into_py(py)).into_py(py),
    }
}

#[pyfunction]
fn write_text(py: Python<'_>, path: &str, value: &str) -> PyObject {
    match std::fs::write(path, value) {
        Ok(_) => LangOk::new(true.into_py(py)).into_py(py),
        Err(error) => LangErr::new(error.to_string().into_py(py)).into_py(py),
    }
}

#[pyfunction]
fn exists(path: &str) -> bool {
    PathBuf::from(path).exists()
}

#[pyfunction]
fn is_file(path: &str) -> bool {
    PathBuf::from(path).is_file()
}

#[pyfunction]
fn is_dir(path: &str) -> bool {
    PathBuf::from(path).is_dir()
}

#[pyfunction]
fn list_dir(py: Python<'_>, path: &str) -> PyObject {
    match std::fs::read_dir(path) {
        Ok(entries) => {
            let names = entries
                .filter_map(|entry| entry.ok())
                .filter_map(|entry| entry.file_name().into_string().ok())
                .map(|name| name.into_py(py))
                .collect::<Vec<_>>();
            LangOk::new(PyList::new_bound(py, names).unbind().into_py(py)).into_py(py)
        }
        Err(error) => LangErr::new(error.to_string().into_py(py)).into_py(py),
    }
}

#[pyfunction]
fn args(py: Python<'_>) -> PyResult<Py<PyList>> {
    let sys = PyModule::import_bound(py, "sys")?;
    let argv = sys.getattr("argv")?.downcast_into::<PyList>()?;
    let values = argv
        .iter()
        .skip(1)
        .map(|item| item.unbind())
        .collect::<Vec<_>>();
    Ok(PyList::new_bound(py, values).unbind())
}

#[pyfunction]
fn read_line(py: Python<'_>) -> PyResult<String> {
    let sys = PyModule::import_bound(py, "sys")?;
    let value = sys.getattr("stdin")?.call_method0("readline")?;
    Ok(value
        .extract::<String>()?
        .trim_end_matches(['\r', '\n'])
        .to_string())
}

#[pyfunction]
fn read_to_end(py: Python<'_>) -> PyResult<String> {
    let sys = PyModule::import_bound(py, "sys")?;
    sys.getattr("stdin")?
        .call_method0("read")?
        .extract::<String>()
}

#[pyfunction(name = "print")]
fn io_print(py: Python<'_>, value: &Bound<'_, PyAny>) -> PyResult<()> {
    let builtins = PyModule::import_bound(py, "builtins")?;
    let kwargs = PyDict::new_bound(py);
    kwargs.set_item("end", "")?;
    builtins.getattr("print")?.call((value,), Some(&kwargs))?;
    Ok(())
}

#[pyfunction]
fn println(py: Python<'_>, value: &Bound<'_, PyAny>) -> PyResult<()> {
    let builtins = PyModule::import_bound(py, "builtins")?;
    builtins.getattr("print")?.call1((value,))?;
    Ok(())
}

#[pyfunction]
fn now() -> PyResult<f64> {
    Ok(SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_err(|error| PyRuntimeError::new_err(error.to_string()))?
        .as_secs_f64())
}

#[pyfunction]
fn sleep(seconds: f64) -> PyResult<()> {
    if seconds < 0.0 {
        return Err(PyValueError::new_err(
            "sleep expected a non-negative duration",
        ));
    }
    std::thread::sleep(std::time::Duration::from_secs_f64(seconds));
    Ok(())
}

#[pyfunction]
fn millis() -> PyResult<u128> {
    Ok(SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_err(|error| PyRuntimeError::new_err(error.to_string()))?
        .as_millis())
}

#[pyfunction]
fn color(value: &str, name: &str) -> String {
    let code = ansi_colors().get(name).copied().unwrap_or("0");
    format!("\x1b[{code}m{value}\x1b[0m")
}

#[pyfunction]
fn bold(value: &str) -> String {
    format!("\x1b[1m{value}\x1b[0m")
}

#[pyfunction]
fn clear() -> &'static str {
    "\x1b[2J\x1b[H"
}

#[pyfunction]
fn prompt(py: Python<'_>, message: &str) -> PyResult<Py<PyAny>> {
    let builtins = PyModule::import_bound(py, "builtins")?;
    builtins
        .getattr("input")?
        .call1((message,))
        .map(|value| value.unbind())
}

fn iterable_to_list<'py>(py: Python<'py>, obj: &Bound<'py, PyAny>) -> PyResult<Bound<'py, PyList>> {
    if let Ok(list) = obj.downcast::<PyList>() {
        return Ok(list.clone());
    }
    let builtins = PyModule::import_bound(py, "builtins")?;
    Ok(builtins
        .getattr("list")?
        .call1((obj,))?
        .downcast_into::<PyList>()?)
}

fn ansi_colors() -> HashMap<&'static str, &'static str> {
    HashMap::from([
        ("black", "30"),
        ("red", "31"),
        ("green", "32"),
        ("yellow", "33"),
        ("blue", "34"),
        ("magenta", "35"),
        ("cyan", "36"),
        ("white", "37"),
        ("bright_black", "90"),
        ("bright_red", "91"),
        ("bright_green", "92"),
        ("bright_yellow", "93"),
        ("bright_blue", "94"),
        ("bright_magenta", "95"),
        ("bright_cyan", "96"),
        ("bright_white", "97"),
    ])
}

#[pymodule]
fn lang_std_native(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<LangSome>()?;
    m.add_class::<LangNothing>()?;
    m.add_class::<LangOk>()?;
    m.add_class::<LangErr>()?;
    m.add_class::<LangIterator>()?;
    m.add_class::<LangPath>()?;

    m.add_function(wrap_pyfunction!(lang_len, m)?)?;
    m.add_function(wrap_pyfunction!(lang_iter, m)?)?;
    m.add_function(wrap_pyfunction!(lang_range, m)?)?;
    m.add_function(wrap_pyfunction!(enumerate, m)?)?;
    m.add_function(wrap_pyfunction!(zip, m)?)?;
    m.add_function(wrap_pyfunction!(map, m)?)?;
    m.add_function(wrap_pyfunction!(filter, m)?)?;
    m.add_function(wrap_pyfunction!(fold, m)?)?;
    m.add_function(wrap_pyfunction!(collect, m)?)?;
    m.add_function(wrap_pyfunction!(any, m)?)?;
    m.add_function(wrap_pyfunction!(all, m)?)?;
    m.add_function(wrap_pyfunction!(panic, m)?)?;
    m.add_function(wrap_pyfunction!(type_name, m)?)?;
    m.add_function(wrap_pyfunction!(debug, m)?)?;
    m.add_function(wrap_pyfunction!(upper, m)?)?;
    m.add_function(wrap_pyfunction!(lower, m)?)?;
    m.add_function(wrap_pyfunction!(title, m)?)?;
    m.add_function(wrap_pyfunction!(strip, m)?)?;
    m.add_function(wrap_pyfunction!(split, m)?)?;
    m.add_function(wrap_pyfunction!(join, m)?)?;
    m.add_function(wrap_pyfunction!(replace, m)?)?;
    m.add_function(wrap_pyfunction!(contains, m)?)?;
    m.add_function(wrap_pyfunction!(starts_with, m)?)?;
    m.add_function(wrap_pyfunction!(ends_with, m)?)?;
    m.add_function(wrap_pyfunction!(vec_ctor, m)?)?;
    m.add_function(wrap_pyfunction!(map_ctor, m)?)?;
    m.add_function(wrap_pyfunction!(set_ctor, m)?)?;
    m.add_function(wrap_pyfunction!(counter_ctor, m)?)?;
    m.add_function(wrap_pyfunction!(lang_min, m)?)?;
    m.add_function(wrap_pyfunction!(lang_max, m)?)?;
    m.add_function(wrap_pyfunction!(lang_abs, m)?)?;
    m.add_function(wrap_pyfunction!(lang_round, m)?)?;
    m.add_function(wrap_pyfunction!(floor, m)?)?;
    m.add_function(wrap_pyfunction!(ceil, m)?)?;
    m.add_function(wrap_pyfunction!(sqrt, m)?)?;
    m.add_function(wrap_pyfunction!(clamp, m)?)?;
    m.add_function(wrap_pyfunction!(rand_fn, m)?)?;
    m.add_function(wrap_pyfunction!(randint, m)?)?;
    m.add_function(wrap_pyfunction!(choice, m)?)?;
    m.add_function(wrap_pyfunction!(shuffle, m)?)?;
    m.add_function(wrap_pyfunction!(cwd, m)?)?;
    m.add_function(wrap_pyfunction!(home, m)?)?;
    m.add_function(wrap_pyfunction!(read_text, m)?)?;
    m.add_function(wrap_pyfunction!(write_text, m)?)?;
    m.add_function(wrap_pyfunction!(exists, m)?)?;
    m.add_function(wrap_pyfunction!(is_file, m)?)?;
    m.add_function(wrap_pyfunction!(is_dir, m)?)?;
    m.add_function(wrap_pyfunction!(list_dir, m)?)?;
    m.add_function(wrap_pyfunction!(args, m)?)?;
    m.add_function(wrap_pyfunction!(read_line, m)?)?;
    m.add_function(wrap_pyfunction!(read_to_end, m)?)?;
    m.add_function(wrap_pyfunction!(io_print, m)?)?;
    m.add_function(wrap_pyfunction!(println, m)?)?;
    m.add_function(wrap_pyfunction!(now, m)?)?;
    m.add_function(wrap_pyfunction!(sleep, m)?)?;
    m.add_function(wrap_pyfunction!(millis, m)?)?;
    m.add_function(wrap_pyfunction!(color, m)?)?;
    m.add_function(wrap_pyfunction!(bold, m)?)?;
    m.add_function(wrap_pyfunction!(clear, m)?)?;
    m.add_function(wrap_pyfunction!(prompt, m)?)?;

    let all = [
        "Some",
        "Nothing",
        "Ok",
        "Err",
        "Iterator",
        "Path",
        "len",
        "iter",
        "range",
        "enumerate",
        "zip",
        "map",
        "filter",
        "fold",
        "collect",
        "any",
        "all",
        "panic",
        "type_name",
        "debug",
        "upper",
        "lower",
        "title",
        "strip",
        "split",
        "join",
        "replace",
        "contains",
        "starts_with",
        "ends_with",
        "Vec",
        "Map",
        "Set",
        "Counter",
        "min",
        "max",
        "abs",
        "round",
        "floor",
        "ceil",
        "sqrt",
        "clamp",
        "rand",
        "randint",
        "choice",
        "shuffle",
        "cwd",
        "home",
        "read_text",
        "write_text",
        "exists",
        "is_file",
        "is_dir",
        "list_dir",
        "args",
        "read_line",
        "read_to_end",
        "print",
        "println",
        "now",
        "sleep",
        "millis",
        "color",
        "bold",
        "clear",
        "prompt",
    ];
    m.add("__all__", all)?;
    let nothing = Py::new(m.py(), LangNothing)?;
    m.add("NOTHING", nothing)?;
    Ok(())
}

#[cfg(all(test, not(windows)))]
mod tests {
    use super::*;

    #[test]
    fn nothing_reports_absence_without_python_runtime() {
        let nothing = LangNothing;
        assert_eq!(nothing.is_some(), false);
        assert_eq!(nothing.is_nothing(), true);
        assert_eq!(nothing.__bool__(), false);
        assert_eq!(nothing.__str__(), "Nothing");
        assert!(nothing.unwrap().is_err());
    }

    #[test]
    fn path_helpers_join_and_display_paths() {
        let path = LangPath::new("root").join(vec!["child".to_string(), "file.txt".to_string()]);
        assert!(path.__str__().contains("root"));
        assert!(path.__str__().contains("child"));
        assert!(path.__repr__().starts_with("Path("));
    }

    #[test]
    fn text_terminal_math_and_random_helpers_work() {
        assert_eq!(upper("lamp"), "LAMP");
        assert_eq!(lower("LAMP"), "lamp");
        assert_eq!(contains("lamp", "am"), true);
        assert_eq!(color("hot", "red"), "\x1b[31mhot\x1b[0m");
        assert_eq!(bold("hot"), "\x1b[1mhot\x1b[0m");
        assert_eq!(clamp(9.0, 1.0, 5.0), 5.0);
        let value = randint(1, 3).unwrap();
        assert!((1..=3).contains(&value));
    }
}

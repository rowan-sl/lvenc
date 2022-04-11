/// Order of elements: x=0 to width, then y=0 to height
#[derive(Debug, Clone, PartialEq)]
pub struct Grid<T> {
    data: Vec<T>,
    dimensions: (usize, usize),
}

impl<T> Grid<T> {
    pub fn filled_with(width: usize, height: usize, elem: T) -> Self
    where
        T: Clone,
    {
        let data = std::iter::repeat(elem)
            .take(width * height)
            .collect::<Vec<_>>();
        Self {
            data,
            dimensions: (width, height),
        }
    }

    pub fn filled_with_fn(width: usize, height: usize, f: impl FnMut() -> T) -> Self {
        let data = std::iter::repeat_with(f)
            .take(width * height)
            .collect::<Vec<_>>();
        Self {
            data,
            dimensions: (width, height),
        }
    }

    /// takes a row major iterator of values, and creates a `Grid`
    pub fn from_iter(width: usize, height: usize, iter: impl Iterator<Item = T>) -> Option<Self> {
        let data = iter.take(width * height).collect::<Vec<T>>();
        if data.len() < width * height {
            None
        } else {
            Some(Self {
                data,
                dimensions: (width, height),
            })
        }
    }

    /// takes a row major iterator of values, and creates a `Grid`
    pub fn from_vec(width: usize, height: usize, data: Vec<T>) -> Option<Self> {
        if data.len() < width * height {
            None
        } else {
            Some(Self {
                data,
                dimensions: (width, height),
            })
        }
    }

    fn index_from_xy(&self, x: usize, y: usize) -> usize {
        self.dimensions.0 * y + x
    }

    pub unsafe fn set_unchecked(&mut self, x: usize, y: usize, elem: T) {
        // num other rows of indexes (width * y) + current index
        let index = self.index_from_xy(x, y);
        *self.data.get_unchecked_mut(index) = elem;
    }

    pub fn set(&mut self, x: usize, y: usize, elem: T) {
        assert!(x < self.dimensions.0 && y < self.dimensions.1);
        unsafe { self.set_unchecked(x, y, elem) }
    }

    pub unsafe fn get_unchecked(&self, x: usize, y: usize) -> T
    where
        T: Clone,
    {
        let index = self.index_from_xy(x, y);
        self.data.get_unchecked(index).clone()
    }

    pub fn get(&self, x: usize, y: usize) -> T
    where
        T: Clone,
    {
        assert!(x < self.dimensions.0 && y < self.dimensions.1);
        unsafe { self.get_unchecked(x, y) }
    }

    pub fn into_row_major_iter(self) -> impl Iterator<Item = (usize, usize, T)> {
        self.data
            .into_iter()
            .enumerate()
            .map(move |(i, t)| (i % self.dimensions.0, i / self.dimensions.0, t))
    }

    pub fn row_major_iter(&self) -> impl Iterator<Item = (usize, usize, &T)> + '_ {
        self.data
            .iter()
            .enumerate()
            .map(move |(i, t)| (i % self.dimensions.0, i / self.dimensions.0, t))
    }

    pub fn into_vec(self) -> Vec<T> {
        self.data
    }

    pub fn transform_into<N>(self, f: impl FnMut(T) -> N) -> Grid<N> {
        let data = self.data.into_iter().map(f).collect::<Vec<N>>();
        Grid::<N> {
            data,
            dimensions: self.dimensions,
        }
    }

    pub fn width(&self) -> usize {
        self.dimensions.0
    }

    pub fn height(&self) -> usize {
        self.dimensions.1
    }
}

#[test]
fn test_grid_indexing() {
    let mut g = Grid::<usize>::filled_with(100, 1, 0);
    for i in 0..100 {
        g.set(i, 0, i);
    }
    for i in 0..100 {
        assert_eq!(i, g.get(i, 0));
    }
}

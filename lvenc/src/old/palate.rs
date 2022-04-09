
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Palate {
    colors: HashMap<Rgb<u8>, usize>,
    latest_id: usize,
}

impl Palate {
    pub fn new() -> Self {
        Self {
            colors: HashMap::new(),
            latest_id: 0,
        }
    }

    pub fn id_of(&self, c: &Rgb<u8>) -> Option<usize> {
        Some(*self.colors.get(c)?)
    }

    pub fn insert_color(&mut self, c: Rgb<u8>) {
        if !self.colors.contains_key(&c) {
            self.colors.insert(c, self.latest_id);
            self.latest_id += 1;
        }
    }

    pub fn reserve(&mut self, additional: usize) {
        self.colors.reserve(additional);
    }
}

pub fn do_a_color_theory(img: &RgbImage, pal: &mut Palate) {
    img.enumerate_pixels().for_each(|pix| {
        pal.insert_color(*pix.2)
    });
}

pub fn main() {
    let mut pal = Palate::new();
    let mut frame = Mat::default();

    let m_b = Instant::now();
    loop {
        if !cap.read(&mut frame)? {
            break;
        }
        assert!(!frame.empty());

        let i = utils::mat_to_image(&frame)?;

        let b = Instant::now();
        do_a_color_theory(&i, &mut pal);
        println!("palateing took {:?}", Instant::now()-b);
    }
    println!("palateing for {} frames took {:?}", cap.get(cv::videoio::CAP_PROP_POS_FRAMES)?, Instant::now()-m_b);

    println!("number of colors: {}", pal.latest_id);

    println!("minimum time to do one frame at 60fps is {}ms", 1f64/60f64*1000f64);
}
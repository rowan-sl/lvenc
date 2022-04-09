use cv::core::Mat_;
use cv::core::Point2i;
use cv::core::Size2i;
use cv::core::Vec3b;
use cv::core::VecN;
use cv::prelude::*;
use opencv as cv;

pub struct ImageIterator<'i> {
    image: &'i Mat_<Vec3b>,
    x: i32,
    y: i32,
    done: bool,
    size: Size2i,
}

impl<'a> Iterator for ImageIterator<'a> {
    type Item = (Point2i, &'a VecN<u8, 3>);

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            None?
        }
        if self.x >= self.size.width {
            self.y += 1;
            self.x = 0;
            if self.y >= self.size.height {
                self.done = true;
                None?
            }
        }
        if let Ok(v) = self.image.at_2d::<Vec3b>(self.x, self.y) {
            Some((
                Point2i {
                    x: self.x,
                    y: self.y,
                },
                v,
            ))
        } else {
            eprintln!("Failed to get pixel");
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (
            (self.size.width * self.size.height) as usize,
            Some((self.size.width * self.size.height) as usize),
        )
    }
}

pub trait IntoImageIterator<'i> {
    fn as_image_iter(&'i self) -> ImageIterator<'i>;
}

impl<'i> IntoImageIterator<'i> for Mat_<Vec3b> {
    fn as_image_iter(&'i self) -> ImageIterator<'i> {
        ImageIterator {
            image: self,
            x: 0,
            y: 0,
            done: false,
            size: self.size().unwrap(),
        }
    }
}

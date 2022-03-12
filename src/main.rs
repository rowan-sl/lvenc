use array2d::Array2D;
use image::Rgb;
use lvenc::utils::mat_to_image;
use opencv::prelude::*;
use opencv::videoio::VideoCapture;

#[derive(Clone, Copy, Debug, PartialEq)]
enum InnerEncodedCell {
    BlackPixel,
    WhitePixel,
    Pixel(image::Rgb<u8>),
}

impl TryFrom<EncodedCell> for InnerEncodedCell {
    type Error = ();
    fn try_from(value: EncodedCell) -> Result<Self, Self::Error> {
        Ok(match value {
            EncodedCell::BlackPixel => Self::BlackPixel,
            EncodedCell::WhitePixel => Self::WhitePixel,
            EncodedCell::Pixel(px) => Self::Pixel(px),
            _ => return Err(()),
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum EncodedCell {
    Empty,

    // first stage of encoding, image is translated to a Array2D of these things
    BlackPixel,
    WhitePixel,
    Pixel(image::Rgb<u8>),

    // second stage of encoding, these things are put into the Array2D where applicable
    UpPtr(InnerEncodedCell),     //same as pixel above
    LeftPtr(InnerEncodedCell),   //same as pixel to the left
    UpLeftPtr(InnerEncodedCell), //same as pixel above and to the left

    // third stage of encoding, the array is flattened, and these are applied
    Repitition,
    Repititions(u8), //u8 is used here because even though some repititions are larger than this, most will not be
}

impl From<InnerEncodedCell> for EncodedCell {
    fn from(value: InnerEncodedCell) -> Self {
        match value {
            InnerEncodedCell::BlackPixel => Self::BlackPixel,
            InnerEncodedCell::WhitePixel => Self::WhitePixel,
            InnerEncodedCell::Pixel(px) => Self::Pixel(px),
        }
    }
}

impl From<image::Rgb<u8>> for EncodedCell {
    fn from(px: image::Rgb<u8>) -> Self {
        if px == image::Rgb([0, 0, 0]) {
            Self::BlackPixel
        } else if px == image::Rgb([255, 255, 255]) {
            Self::WhitePixel
        } else {
            Self::Pixel(px)
        }
    }
}

fn main() {
    let mut cap = VideoCapture::from_file("salmon_cannon.mp4", opencv::videoio::CAP_ANY).unwrap();

    let mut frame = Mat::default();

    assert!(cap.read(&mut frame).unwrap());
    let img = mat_to_image(&frame).unwrap();

    let mut arr = Array2D::filled_with(
        EncodedCell::Empty,
        img.width() as usize,
        img.height() as usize,
    );
    dbg!(img.width());
    dbg!(img.height());

    for pix in img.enumerate_pixels() {
        let x = pix.0 as usize;
        let y = (img.height() - pix.1) as usize - 1;
        let content = pix.2;

        let above = arr.get(x, y + 1);
        let left = if x > 0 { arr.get(x - 1, y) } else { None };
        let above_left = if x > 0 { arr.get(x - 1, y + 1) } else { None };

        let at_xy = arr.get(x, y).unwrap();

        let mut new_value: Option<EncodedCell> = None;

        if let EncodedCell::Empty = at_xy {
            new_value = Some((*content).into());
        } else {
            panic!("deja vu")
        }

        if let Some(lft) = left {
            encoded_converter!(lft, content, EncodedCell::LeftPtr, new_value);
        }
        if let Some(abv) = above {
            encoded_converter!(abv, content, EncodedCell::UpPtr, new_value);
        }
        if let Some(lft_abv) = above_left {
            encoded_converter!(lft_abv, content, EncodedCell::UpLeftPtr, new_value);
        }
    }
}

#[macro_export]
macro_rules! encoded_converter {
    ($cell:expr, $content:ident, $direction:expr, $new_value:ident) => {
        if EncodedCell::from(*$content) == *$cell {
            $new_value = Some($direction((*$cell).try_into().unwrap()));
        } else if let EncodedCell::LeftPtr(item)
        | EncodedCell::UpLeftPtr(item)
        | EncodedCell::UpPtr(item) = *$cell
        {
            let pointed_to: EncodedCell = item.into();
            if pointed_to == *$cell {
                $new_value = Some($direction((*$cell).try_into().unwrap()));
            }
        }
    };
}

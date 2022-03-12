use cv::core::VecN;
use cv::prelude::*;
use opencv as cv;

use image::RgbImage;

fn flatten_vecnu83(v: &[VecN<u8, 3>]) -> Vec<u8> {
    let mut res = Vec::with_capacity(v.len() * 3);
    for vecn in v {
        for elem in vecn.as_slice().iter().rev() {
            res.push(*elem);
        }
    }
    res
}

#[derive(thiserror::Error, Debug)]
pub enum MatToImageError {
    #[error("Could not convert the generic mat to the correct data type:\n{0}")]
    BadDataType(opencv::Error),

    #[error("Could not get the size of the mat:\n{0}")]
    GetSizeError(opencv::Error),
}

pub fn mat_to_image(m: &Mat) -> Result<RgbImage, MatToImageError> {
    let size = match m.size() {
        Ok(s) => s,
        Err(e) => {
            return Err(MatToImageError::GetSizeError(e));
        }
    };
    let typed_dat = match m.data_typed::<VecN<u8, 3>>() {
        Ok(typed_dat) => typed_dat,
        Err(e) => {
            return Err(MatToImageError::BadDataType(e));
        }
    };
    Ok(
        match RgbImage::from_vec(
            size.width as u32,
            size.height as u32,
            flatten_vecnu83(typed_dat),
        ) {
            Some(image) => image,
            None => {
                panic!("The buffer was not large enough to fill a the size provided by mat.size(). this should not happen, and is a bug");
            }
        },
    )
}

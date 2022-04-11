#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum PacketMetadata {
    /// data vector can be ignored, should be empty
    Init {
        /// width, height
        frame_size: (u32, u32),
    },
    /// data vector includes some data, for the next frame.
    NewFrame(usize /* amnt of bytes for the next frame */),
    /// data vector includes some data
    FrameData,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Packet {
    pub(crate) metadata: PacketMetadata,
    pub(crate) data: Vec<u8>,
}

impl Packet {
    pub fn len(&self) -> usize {
        self.data.len()
    }
}

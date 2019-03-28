mod ids {
    use super::*;
    use once_cell::sync::OnceCell;

    #[derive(Debug, Copy, Clone)]
    struct NetlinkIds {
        family_id: u16,
        group_id: u32,
    }

    static IDS: OnceCell<NetlinkIds> = OnceCell::INIT;

    fn fetch_ids(sock: &mut Socket) -> Result<NetlinkIds> {
        if let Some(&ids) = IDS.get() {
            Ok(ids)
        } else {
            let family = get_generic_family(sock, "acpi_event")?;
            let group_id = family
                .multicast_groups
                .iter()
                .find(|&group| &group.name == "acpi_mc_group")
                .ok_or(ErrorKind::GroupNotFound)?
                .id;

            let ids = NetlinkIds {
                family_id: family.id,
                group_id,
            };

            // Ignore the result of this; it doesn't matter if
            // somebody else set the cached value first, because the
            // group/family ids shouldn't change anyway
            IDS.set(ids).ok();

            Ok(ids)
        }
    }

    pub fn mcast_group_id(sock: &mut Socket) -> Result<u32> {
        fetch_ids(sock).map(|ids| ids.group_id)
    }

    pub fn family_id(sock: &mut Socket) -> Result<u16> {
        fetch_ids(sock).map(|ids| ids.family_id)
    }
}

/// Represents an ACPI event structure.
#[derive(Debug)]
struct AcpiEvent {
    pub device_class: CString,
    pub bus_id: CString,
    pub evt_type: u32,
    pub data: u32,
}

impl From<Vec<u8>> for AcpiEvent {
    fn from(bytes: Vec<u8>) -> AcpiEvent {
        /// The structure of the raw data provided by the ACPI netlink
        /// socket
        #[repr(C)]
        struct RawAcpiEvent {
            pub device_class: [u8; 20],
            pub bus_id: [u8; 15],
            pub evt_type: u32,
            pub data: u32,
        }

        /// Creates a CString from a byte slice which may contain null
        /// characters, only copying once.
        fn cstring_from_byte_slice(bytes: &[u8]) -> CString {
            let nonzero_bytes = bytes.iter().take_while(|&&c| c != 0).count();
            // Allocate one extra byte, because CString will want to
            // push a null character onto the end
            let mut bytes_copy = Vec::with_capacity(nonzero_bytes + 1);

            bytes_copy.extend_from_slice(&bytes[..nonzero_bytes]);

            // This is allowed because we just chopped off all the
            // zero bytes; also, it won't reallocate because there's
            // room
            unsafe { CString::from_vec_unchecked(bytes_copy) }
        }

        impl Into<AcpiEvent> for &RawAcpiEvent {
            fn into(self) -> AcpiEvent {
                AcpiEvent {
                    device_class: cstring_from_byte_slice(&self.device_class),
                    bus_id: cstring_from_byte_slice(&self.bus_id),
                    evt_type: self.evt_type,
                    data: self.data,
                }
            }
        }

        let raw_event = &*unsafe {
            Box::from_raw(Box::into_raw(bytes.into_boxed_slice()) as *mut RawAcpiEvent)
        };

        raw_event.into()
    }
}

const ACPI_EVENT_IDENTIFIER: u16 = 1;

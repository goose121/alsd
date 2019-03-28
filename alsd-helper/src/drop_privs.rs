// This file is part of alsd.
//
// alsd is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// alsd is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with alsd. If not, see <https://www.gnu.org/licenses/>.

//! A module for dropping root privileges. I am not using `privdrop`
//! because it doesn't set supplementary group IDs, which is a
//! possible security vulnerability, and it isn't very parameterizable
//! because I don't need it to be.
use crate::errors::*;
use nix::unistd::*;

fn do_chroot() -> Result<()> {
    chroot("/var/empty")?;
    chdir("/").map_err(Into::into)
}

fn set_ids() -> Result<()> {
    let gid = getgid();
    setgroups(&[gid])?;
    setgid(gid)?;
    setuid(getuid()).map_err(Into::into)
}

pub fn drop_privs() -> Result<()> {
    do_chroot().and_then(|_| set_ids())
}

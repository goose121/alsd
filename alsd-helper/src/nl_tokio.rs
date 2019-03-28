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

//! This module provides a tokio `Stream` interface to
//! `netlink_rust`'s `Socket` type, which reads and buffers the
//! messages on the socket, providing them individually.

use mio::event::Evented;
use mio::unix::EventedFd;
use mio::{Poll as MioPoll, PollOpt, Ready, Token};
use netlink_rust::{Socket, Message};
use std::io;
use std::os::unix::io::AsRawFd;
use tokio::prelude::Stream;
use tokio_reactor::Registration;
use std::collections::VecDeque;
use futures::{Async, Poll};
use crate::{Result, Error};

struct EventedSocket(Socket);

impl Evented for EventedSocket {
    fn register(&self, poll: &MioPoll, token: Token, interest: Ready, opts: PollOpt) -> io::Result<()> {
        EventedFd(&self.0.as_raw_fd()).register(poll, token, interest, opts)
    }

    fn reregister(&self, poll: &MioPoll, token: Token, interest: Ready, opts: PollOpt) -> io::Result<()> {
        EventedFd(&self.0.as_raw_fd()).reregister(poll, token, interest, opts)
    }

    fn deregister(&self, poll: &MioPoll) -> io::Result<()> {
        EventedFd(&self.0.as_raw_fd()).deregister(poll)
    }
}

pub struct AsyncNlSocket {
    sock: EventedSocket,
    reg: Registration,
    msg_buf: VecDeque<Message>
}

impl AsyncNlSocket {
    pub fn new(sock: Socket) -> Result<AsyncNlSocket> {
        let sock = EventedSocket(sock);
        let reg = Registration::new();
        reg.register(&sock)?;

        Ok(
            AsyncNlSocket {
                sock,
                reg,
                msg_buf: VecDeque::new()
            })
    }
}

impl Stream for AsyncNlSocket {
    type Item = Message;
    type Error = Error;

    fn poll(&mut self) -> Poll<Option<Self::Item>, Self::Error> {
        if let Some(msg) = self.msg_buf.pop_front() {
            Ok(Async::Ready(Some(msg)))
        } else if try_ready!(self.reg.poll_read_ready()).is_readable() {
            self.msg_buf = self.sock.0.receive_messages()?.into();

            if self.msg_buf.is_empty() {
                Ok(Async::NotReady)
            } else {
                // This won't recurse beyond the one level because
                // it'll find the message in the first branch
                self.poll()
            }
        } else {
            Ok(Async::NotReady)
        }
    }
}

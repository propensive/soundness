When transmitting binary data, we sometimes need to present it in a
textual form for human-readibility or to embed within another textual form, such
as XML.

The most common formats for this are hexadecimal and BASE-64, but BASE-32, octal
and binary are also used. A number of variants of these formats exist, optimised
for different scenarios.

_Monotonous_ provides a consistent API for serializing binary data into these
formats.

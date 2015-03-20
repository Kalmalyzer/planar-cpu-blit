# Planar CPU blit operations for Amiga

Here are some CPU-based bitmap blitting operations. There is a masked and a non-masked version available.

They will perform strictly 32-bit aligned memory accesses. This enables high performance blits both fastmem->fastmem and fastmem->chipmem on Amiga 1200/4000.

/*
 * This source file was generated by the Gradle 'init' task
 */
package day9;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

import java.util.List;

class AppTest {
    @Test void parser() {
        DiskMap diskMap = new DiskMap();
        assertEquals("00...111...2...333.44.5555.6666.777.888899", String.join("", diskMap.parse("2333133121414131402")));
    }

    @Test void compact() {
        DiskMap diskMap = new DiskMap();
        List<String>blocks = diskMap.parse("2333133121414131402");
        assertEquals("0099811188827773336446555566..............", String.join("", diskMap.makeCompact(blocks)));
    }

    @Test void checksum() {
        DiskMap diskMap = new DiskMap();
        List<String> blocks = diskMap.parse("2333133121414131402");
        assertEquals(1928, diskMap.checksum(blocks));
    }

    @Test void defragment() {
        DiskMap diskMap = new DiskMap();
        List<String> blocks = diskMap.parse("2333133121414131402");
        assertEquals("00992111777.44.333....5555.6666.....8888..", String.join("", diskMap.defragment(blocks)));
    }
}

package day9;

import java.io.File;
import java.util.List;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;

record DiskMap() {

    static int lastId = 0;
    static HashMap<Integer, Integer> blockSizes = new HashMap<>();
    static HashMap<Integer, Integer> blockIndices = new HashMap<>();

    List<String> parse(String input) {
        List<String> result = new ArrayList<>();
        int id = 0;
        boolean odd_i = true;
        for (int i = 0; i < input.length(); i++) {
            char c = input.charAt(i);
            int times = Character.getNumericValue(c);
            if (odd_i) {
                for (int j = 0; j < times; j++) {
                    result.add(String.valueOf(id));
                }
                blockSizes.put(id, times);
                blockIndices.put(id, result.size() - times);
                id++;
            } else {
                for (int j = 0; j < times; j++) {
                    result.add(".");
                }
            }
            odd_i = !odd_i;
        }
        lastId = id - 1;
        return result;
    }

    private List<String> swap(List<String> blocks, int i, int j) {
        String temp = blocks.get(i);
        blocks.set(i, blocks.get(j));
        blocks.set(j, temp);
        return blocks;
    }

    List<String> makeCompact(List<String> b) {
        List<String> blocks = new ArrayList<>(b);
        int left = 0;
        int right = blocks.size() - 1;
        while (left < right) {
            while (left < right && !blocks.get(left).equals(".")) {
                left++;
            }
            while (left < right && blocks.get(right).equals(".")) {
                right--;
            }
            if (left < right) {
                blocks = swap(blocks, left, right);
                left++;
                right--;
            }
        }
        return blocks;
    }

    Long checksum(List<String> b) {
        List<String> blocks = new ArrayList<>(b);
        Long checksum = 0L;

        for (int i = 0; i < blocks.size(); i++) {
            String block = blocks.get(i);
            if (!block.equals(".")) {
                checksum += (long) i * Integer.parseInt(block);
            }
        }

        return checksum;

    }

    List<String> defragment(List<String> b) {
        List<String> blocks = new ArrayList<>(b);
        for (int id = lastId; id >= 0; id--) {
            int blockSize = blockSizes.get(id);
            int blockIndex = blockIndices.get(id);

            for (int j = 0; j < blockIndex; j++) {
                if (blocks.get(j).equals(".")) {
                    int freeSpace = 0;
                    for (int k = j; k < blocks.size(); k++) {
                        if (blocks.get(k).equals(".")) {
                            freeSpace++;
                        } else {
                            break;
                        }
                    }
                    if (freeSpace >= blockSize) {
                        for (int k = 0; k < blockSize; k++) {
                            blocks.set(j + k, String.valueOf(id));
                        }
                        for (int k = blockIndex; k < blockIndex + blockSize; k++) {
                            blocks.set(k, ".");
                        }
                        break;
                    }
                }
            }
        }
        return blocks;
    }
}

record Puzzle(String input) {
    static String getInput() {
        File file = new File("input.txt");
        try {
            return new String(java.nio.file.Files.readAllBytes(file.toPath()));
        } catch (Exception e) {
            e.printStackTrace();
        }
        return "";
    }

    public static void main(String[] args) {
        DiskMap diskMap = new DiskMap();
        var parsed = Collections.unmodifiableList(
                diskMap.parse(Puzzle.getInput()));

        // Part 1
        System.out.println(diskMap.checksum(diskMap.makeCompact(parsed)));

        // Part 2
        System.out.println(diskMap.checksum(diskMap.defragment(parsed)));
    }
}
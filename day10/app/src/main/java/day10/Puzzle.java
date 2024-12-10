package day10;

import java.io.File;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

record HikingMap() {
    static Grid grid;

    record Point(Integer[] coordinates, Integer value) {
        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder();
            sb.append("(");
            for (int i = 0; i < coordinates.length; i++) {
                sb.append(coordinates[i]);
                if (i < coordinates.length - 1) {
                    sb.append(", ");
                }
            }
            sb.append(") -> ").append(value);
            return sb.toString();
        }
    }

    record NaryTree(Point point, LinkedList<NaryTree> children) {
        static NaryTree root;

        NaryTree addChild(Point point) {
            var child = new NaryTree(point, new LinkedList<>());
            children.add(child);
            return child;
        }

        List<NaryTree> getLeafs() {
            return getLeafs(new LinkedList<>());
        }

        List<NaryTree> getLeafs(LinkedList<NaryTree> leafs) {
            if (!children.isEmpty()) {
                for (var child : children) {
                    child.getLeafs(leafs);
                }
            } else {
                leafs.add(this);
            }
            return leafs;
        }
    }

    record Grid() {

        static Point[][] rows;
        static HashMap<Integer, List<Point>> locations = new HashMap<>();

        public Grid(String input) {
            this();

            var lines = input.split("\n");
            rows = new Point[lines.length][];

            for (int i = 0; i < lines.length; i++) {
                var line = lines[i];
                var numbers = line.split("");
                Point[] row = new Point[numbers.length];
                for (int j = 0; j < numbers.length; j++) {
                    int val = Integer.parseInt(numbers[j]);
                    row[j] = new Point(new Integer[] { i, j }, val);
                    Point p = new Point(new Integer[] { i, j }, val);

                    if (locations.containsKey(val)) {
                        locations.get(val).add(p);
                    } else {
                        locations.put(val, new LinkedList<>());
                        locations.get(val).add(p);
                    }
                }
                rows[i] = row;
            }
        }

        Point get(Integer[] coords) {
            return rows[coords[0]][coords[1]];
        }

        Integer[] dim() {
            return new Integer[] { rows.length, rows[0].length };
        }

        List<Point> find(int value) {
            return locations.get(value);
        }

        List<Point> getNeighbors(Point point) {
            var neighbors = new LinkedList<Point>();
            int x = point.coordinates()[0];
            int y = point.coordinates()[1];
            Integer[] dim = dim();
            int rows = dim[0];
            int cols = dim[1];

            if (x - 1 >= 0) {
                Integer[] coords = new Integer[] { x - 1, y };
                neighbors.add(new Point(coords, get(coords).value()));
            }
            if (x + 1 < rows) {
                Integer[] coords = new Integer[] { x + 1, y };
                neighbors.add(new Point(coords, get(coords).value()));
            }
            if (y - 1 >= 0) {
                Integer[] coords = new Integer[] { x, y - 1 };
                neighbors.add(new Point(coords, get(coords).value()));
            }
            if (y + 1 < cols) {
                Integer[] coords = new Integer[] { x, y + 1 };
                neighbors.add(new Point(coords, get(coords).value()));
            }
            return neighbors;

        }
    }

    public HikingMap(String input) {
        this();
        grid = new Grid(input);
    }

    List<NaryTree> getTrailheads() {
        var zeros = grid.find(0);
        var trailheads = new LinkedList<NaryTree>();
        for (var zero : zeros) {
            trailheads.add(getPaths(zero));
        }
        return trailheads;
    }

    NaryTree getPaths(Point point) {
        var paths = new LinkedList<NaryTree>();

        var nextSteps = grid
                .getNeighbors(point)
                .stream()
                .filter(n -> grid.get(n.coordinates()).value() == point.value() + 1)
                .toList();

        for (var nextStep : nextSteps) {
            paths.add(getPaths(nextStep));
        }

        return new NaryTree(grid.get(point.coordinates()), paths);
    }

    Integer getScore(boolean distinct) {
        int score = 0;
        for (var trailhead : getTrailheads()) {
            var peaks = trailhead
                    .getLeafs()
                    .stream()
                    .filter(l -> l.point().value() == 9);

            if (distinct) {
                score += peaks.distinct().count();
            } else {
                score += peaks.count();
            }
        }
        return score;
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
        var map = new HikingMap(Puzzle.getInput());

        // part 1
        System.out.println(map.getScore(true));

        // part 2
        System.out.println(map.getScore(false));
    }
}

# Binary Search Tree
 A C++17 implementation of an [AVL-Tree](https://en.wikipedia.org/wiki/AVL_tree) that can be used either as a **Key** set or as a **Key-Value** map.
 
 It covers most of what [std::set](https://en.cppreference.com/w/cpp/container/set) and [std::map](https://en.cppreference.com/w/cpp/container/map) offer
 
 As a **Key** set:
 ```c++
 using namespace containers::trees;
 
 // (1) Default constructor
 BinarySearchTree<std::string> a{};
 
 a.add("CAT");
 a["DOG"];
 a.add({"HORSE", "FROG", "CROCODILE"});
 
 std::cout << "(1) Default constructor: ";
 
 for(const auto& node : a) std::cout << node.get_key() << ' ';
 std::cout << "\n----------------\n";
 
 // (2) Iterator constructor
 BinarySearchTree<std::string> b(a.find("DOG"), a.end());
 
 std::cout << "(2) Iterator constructor: ";
 
 for(const auto& node : b) std::cout << node.get_key() << ' ';
 std::cout << "\n----------------\n";
 
 // (3) Copy constructor
 BinarySearchTree<std::string> c(a);
 
 c.add("ANOTHER HORSE");
 
 std::cout << "(3) Copy constructor: ";
 
 for(const auto& node : c) std::cout << node.get_key() << ' ';
 std::cout << "\n----------------\n";
 
 // (4) Move constructor
 BinarySearchTree<std::string> d(std::move(a));
 
 std::cout << "(4) Move constructor: ";
 
 for(const auto& node : d) std::cout << node.get_key() << ' ';
 std::cout << '\n';
 
 std::cout << "'a' after moving: ";
 for(const auto& node : a) std::cout << node.get_key() << ' ';
 std::cout << "\n----------------\n";
 
 // (5) Initializer list constructor
 BinarySearchTree<std::string> e {"ONE", "TWO", "THREE", "FIVE", "EIGHT"};
 
 std::cout << "(5) Initializer list constructor: ";
 
 for(const auto& node : e) std::cout << node.get_key() << ' ';
 std::cout << "\n----------------\n";
 
 // custom comparison
 struct Point
 {
 	double x, y;
 
 	bool operator==(const Point& other) const
 	{
 		return std::hypot(x, y) == std::hypot(other.x, other.y);
 	}
 };
 
 struct PointCmp {
 
 	bool operator()(const Point& lhs, const Point& rhs) const
 	{
 		return std::hypot(lhs.x, lhs.y) < std::hypot(rhs.x, rhs.y);
 	}
 
 };
 
 BinarySearchTree<Point, PointCmp> z {{2, 5}, {3, 4}, {1, 1}};
 
 z[{1, -1}]; // this doesn't add because the magnitude of (1,-1) equals (1,1)
 
 std::cout << "(6) custom comparison: ";
 
 for(const auto& node : z) std::cout << '(' << node.get_key().x << ',' << node.get_key().y << ") ";
 std::cout << '\n';
 ```
 
 As a **Key-Value** map:
 ```c++
 using namespace containers::trees;
 
 auto print_map = [](const auto& tree)
 {
 	std::string output{};
 	std::ostringstream oss{};
 
 	std::cout << '{';
 
 	for(const auto& node : tree) {
 
 		if(!output.empty()) output += ", ";
 
 		oss << node.get_key() << " : " << node.get_value().value();
 
 		output += oss.str();
 
 		oss.str({});
 
 	}
 
 	std::cout << output << "}\n";
 };
 
 // (1) Default constructor
 
 // Using helper function
 auto map1 = make_asc_bst<std::string, int>(); // or 'make_bst' for default ascending order, or 'make_desc_bst' for descending order
 
 map1["SOMETHING"]  = 69;
 map1["ANYTHING"]   = 199;
 map1["THAT THING"] = 50;
 
 std::cout << "(1) Default constructor:\nmap1 = "; print_map(map1);
 
 std::cout << "----------------\n";
 
 // (2) Range constructor
 auto iter = make_asc_bst<std::string, int>(map1.find("ANYTHING"), map1.end());
 
 std::cout << "(2) Range constructor:\niter = "; print_map(iter);
 std::cout << "\nmap1 = "; print_map(map1);
 
 std::cout << "----------------\n";
 
 // (3) Copy constructor
 auto copied = make_asc_bst<std::string, int>(map1);
 
 std::cout << "(3) Copy constructor:\ncopied = "; print_map(copied);
 std::cout << "\nmap1 = "; print_map(map1);
 
 std::cout << "----------------\n";
 
 // (4) Move constructor
 auto moved = make_asc_bst<std::string, int>(std::move(map1));
 
 std::cout << "(4) Move constructor:\nmoved = "; print_map(moved);
 std::cout << "\nmap1 = "; print_map(map1);
 
 std::cout << "----------------\n";
 
 // (5) Initializer list constructor
 const auto init = make_asc_bst<std::string, int>({
 
 	{"this", 100},
 	{"can", 100},
 	{"be", 100},
 	{"const", 100},
 
 });
 
 std::cout << "(5) Initializer list constructor:\ninit = "; print_map(init);
 
 std::cout << "----------------\n";
 
 struct Point
 {
 	double x, y;
 
 	bool operator==(const Point& other) const
 	{
 		return std::hypot(x, y) == std::hypot(other.x, other.y);
 	}
 };
 
 struct PointCmp
 {
 	bool operator()(const Point& lhs, const Point& rhs) const
 	{
 		return lhs.x < rhs.x; // NB. intentionally ignores y
 	}
 };
 
 // Custom Key class option 1:
 // Use a comparison struct
 auto mag = make_bst<Point, double, PointCmp>({
 
 	{ {5, -12}, 13 },
 	{ {3, 4},   5 },
 	{ {-8, -15}, 17 }
 
 });
 
 std::cout << "(6) Comparison struct:\n";
 
 for(const auto& node : mag) {
 
 	std::cout << "The magnitude of (" << node.get_key().x
 			  << ", " << node.get_key().y << ") is "
 			  << *node.get_value() << '\n';
 
 }
 
 std::cout << "----------------\n";
 
 // Custom Key class option 2:
 // Use a comparison lambda
 // This lambda sorts points according to their magnitudes, where note that
 //  these magnitudes are taken from the local variable mag
 auto cmp_lambda = [&mag](const Point& lhs, const Point& rhs) { return mag[lhs] < mag[rhs]; };
 
 //You could also use a lambda that is not dependent on local variables, like this:
 //auto cmp_lambda = [](const Point& lhs, const Point& rhs) { return lhs.y < rhs.y; };
 auto magy = make_bst<Point, double, decltype(cmp_lambda)>(cmp_lambda);
 
 //Various ways of inserting elements:
 magy.add(std::pair<Point, double>({5, -12}, 13));
 magy.add({ {3, 4}, 5});
 magy.add({Point{-8.0, -15.0}, 17});
 magy[{3, 0}] = 3;
 
 std::cout << "(7) Comparison lambda:\n";
 
 for(const auto& node : magy) {
 
 	std::cout << "The magnitude of (" << node.get_key().x
 			  << ", " << node.get_key().y << ") is "
 			  << *node.get_value() << '\n';
 
 }
 ```
 
 Please see the **Main.cpp** file for more details.

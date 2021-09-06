#include <iostream>
#include <iomanip>
#include <stdlib.h>
#include <string>

#include "BinarySearchTree.h"

using namespace containers::trees;

void asc_and_desc()
{
    std::cout << "-------------asc_and_desc------------\n" << std::endl;

    auto foods = make_asc_bst<std::string>({"PIE", "TEMAKI", "CHEESE"});

    foods["PIZZA"];
    foods["LASAGNA"];
    foods["BREAD"];

    foods.add("SALAD");
    foods.add({"RICE", "BEAN", "LEMON"});

    std::cout << std::left;

    constexpr int width = 7;

    for(const auto& node : foods) std::cout << std::setw(width) << node.get_key() << " : " << node.get_value().value_or("NULL") << std::endl;

    auto r_foods = make_desc_bst<std::string>(foods.cbegin(), foods.cend());

    std::cout << "\n-----------\n" << std::endl;

    for(auto cit = r_foods.cbegin(); cit != r_foods.cend(); ++cit) std::cout << std::setw(width) << cit->get_key() << " : " << cit->get_value().value_or("NULL") << std::endl;

    std::cout << "\n-----------\n" << std::endl;

    std::cout << "Has PIZZA? " << (foods.contains("PIZZA") ? "Yes!" : "No!") << std::endl;

    std::cout << "\n-------------------------------------\n" << std::endl;
}

void asc_and_desc_pairs()
{
    std::cout << "----------asc_and_desc_pairs---------\n" << std::endl;

    auto polygons = make_asc_bst<std::string, unsigned>({

        {"TRIANGLE", 3},
        {"SQUARE"  , 4},
        {"PENTAGON", 5}

    });

    polygons["HEXAGON"]  = 6;
    polygons["HEPTAGON"] = 7;
    polygons["OCTAGON"]  = 8;

    polygons.add(polygons.make_kv("DECAGON", 10));

    polygons.add({

        polygons.make_kv("HENDECAGON", 11),
        polygons.make_kv("DODECAGON" , 12),
        polygons.make_kv("TRIDECAGON", 13)

    });

    auto it = polygons.find("DODECAGON");

    if(it != polygons.end()) {

        std::cout << "A " << it->get_key() << " has " << *it->get_value() << " sides" << std::endl;

        std::cout << "\n-----------\n" << std::endl;

    }

    std::cout << std::left;

    constexpr int width = 10;

    for(const auto& node : polygons) std::cout << std::setw(width) << node.get_key() << " : " << node.get_value().value_or(0) << std::endl;

    auto r_polygons = make_bst<std::string, unsigned, std::greater<std::string>>();

    r_polygons.add(polygons.cbegin(), polygons.cend());

    std::cout << "\n-----------\n" << std::endl;

    for(auto cit = r_polygons.cbegin(); cit != r_polygons.cend(); ++cit) std::cout << std::setw(width) << cit->get_key() << " : " << cit->get_value().value_or(0) << std::endl;

    std::cout << "\n-----------\n" << std::endl;

    std::cout << "Has NONAGON? ";

    try {

        polygons.at("NONAGON");

        std::cout << "Yes!" << std::endl;

    }catch(const std::out_of_range&) {

        std::cout << "No!" << std::endl;

    }

    std::cout << "\n-----------\n" << std::endl;

    BinarySearchTree polygons_copy { // Deduction Guide (C++17)

        std::pair{std::string("SQUARE"), 4u},

        {"NONAGON", 9},

        {"OCTAGON", 8}
    };

    // Copy by Nodes is also possible
    for(const decltype(polygons)::Node& node : polygons) polygons_copy.add(node);

    std::cout << "polygons == polygons_copy ? " << std::boolalpha << (polygons == polygons_copy) << std::endl;

    std::cout << "\n-------------------------------------\n" << std::endl;
}

int main()
{
    asc_and_desc();

    asc_and_desc_pairs();

    return EXIT_SUCCESS;
}

#include <iostream>

int main(void) {

    std::string in;
    unsigned int chunk_size;   

    std::cout << "Enter the string: ";
    std::cin >> in;
    std::cout << "Enter the chunk size: ";
    std::cin >> chunk_size;

    std::vector<std::string> chunks;

    for (size_t i = 0; i < in.size(); i += chunk_size) {
        chunks.push_back(in.substr(i, chunk_size));
    }

    std::cout << "Chunks: " << std::endl;
    for (const auto& chunk : chunks) {
        std::cout << chunk << std::endl;
    }
}
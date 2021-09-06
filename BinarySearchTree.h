/****************************************************************************************/
/* Copyright (c) 2021 Muller Castro                                                     */
/*                                                                                      */
/* Permission is hereby granted, free of charge, to any person obtaining                */
/* a copy of this software and associated documentation files (the "Software"),         */
/* to deal in the Software without restriction, including without limitation            */
/* the rights to use, copy, modify, merge, publish, distribute, sublicense,             */
/* and/or sell copies of the Software, and to permit persons to whom the Software       */
/* is furnished to do so, subject to the following conditions:                          */
/*                                                                                      */
/* The above copyright notice and this permission notice shall be included in all       */
/* copies or substantial portions of the Software.                                      */
/*                                                                                      */
/* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,  */
/* INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A        */
/* PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT   */
/* HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF */
/* CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE */
/* OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                        */
/****************************************************************************************/

#ifndef BINARY_SEARCH_TREE_H
#define BINARY_SEARCH_TREE_H

static_assert(__cplusplus >= 201703L, "The 'BinarySearchTree.h' library must be used with at least C++17");

#include <optional>
#include <memory>
#include <functional>
#include <type_traits>
#include <utility>
#include <vector>
#include <stack>
#include <iterator>
#include <queue>
#include <algorithm>
#include <stdexcept>

namespace containers::trees {

    template<
        typename Key,
        typename Compare = std::less<Key>,
        typename Value   = Key
    >
    class BinarySearchTree final
    {
    public:
        using KeyType      = Key;
        using ValueType    = Value;
        using KeyValueType = std::pair<KeyType, std::optional<ValueType>>;
        using CompareType  = Compare;

        struct Node final
        {
            Node(Node* parent_, const KeyValueType& kv) : parent{parent_}, balance_factor{}, key(kv.first), value(kv.second), left{}, right{} {}

            Node(Node* parent_, KeyValueType&& kv) : parent{parent_}, balance_factor{}, key(std::move(kv.first)), value(std::move(kv.second)), left{}, right{} {}

            Node(Node* parent_, const KeyType& key_, const std::optional<ValueType>& value_) : parent{parent_}, balance_factor{}, key(key_), value(value_), left{}, right{} {}

            Node(Node* parent_, const KeyType& key_, std::optional<ValueType>&& value_) : parent{parent_}, balance_factor{}, key(key_), value(std::move(value_)), left{}, right{} {}

            Node(Node* parent_, KeyType&& key_, std::optional<ValueType>&& value_) : parent{parent_}, balance_factor{}, key(std::move(key_)), value(std::move(value_)), left{}, right{} {}

            Node(const Node&) = delete;

            Node(Node&&) = delete;

            Node& operator=(const Node&) = delete;

            Node& operator=(Node&&) = delete;

            bool operator==(const Node& other) const
            {
                return (key == other.key) && (value == other.value);
            }

            bool operator!=(const Node& other) const
            {
                return (key != other.key) && (value != other.value);
            }

            bool operator<(const Node& other) const
            {
                return (key < other.key) && (value < other.value);
            }

            bool operator>(const Node& other) const
            {
                return (key > other.key) && (value > other.value);
            }

            bool operator<=(const Node& other) const
            {
                return (key <= other.key) && (value <= other.value);
            }

            bool operator>=(const Node& other) const
            {
                return (key >= other.key) && (value >= other.value);
            }

            [[nodiscard]] operator KeyValueType() const { return KeyValueType(key, value); }

            [[nodiscard]] constexpr const KeyType& get_key() const noexcept { return key; }

            [[nodiscard]] constexpr const std::optional<ValueType>& get_value() const noexcept { return value; }

            [[nodiscard]] constexpr std::optional<ValueType>& get_value() noexcept { return value; }

        private:
            friend class BinarySearchTree;

            Node* parent;

            signed char balance_factor;

            KeyType key;

            std::optional<ValueType> value;

            std::unique_ptr<Node> left, right;
        };

        class BidirectionalImpl
        {
        protected:
            template<typename NodePointer>
            static void next(NodePointer* current_node, const BinarySearchTree* tree)
            {
                if(*current_node == tree->rightmost) {

                    return;

                }else if(*current_node == (tree->rightmost - 1)) {

                    ++(*current_node);

                    return;

                }else if(!(*current_node) && !tree->is_empty()) {

                    *current_node = tree->rightmost - 1;

                    return;

                }

                if((*current_node)->right) {

                    if((*current_node)->right->left) {

                        *current_node = (*current_node)->right->left.get();

                        while((*current_node)->left) *current_node = (*current_node)->left.get();

                    }else {

                        *current_node = (*current_node)->right.get();

                    }

                }else if((*current_node)->parent) {

                    while(*current_node == (*current_node)->parent->right.get()) *current_node = (*current_node)->parent;

                    *current_node = (*current_node)->parent;

                }
            }

            template<typename NodePointer>
            static void previous(NodePointer* current_node, const BinarySearchTree* tree)
            {
                if(*current_node == tree->leftmost) {

                    return;

                }else if(*current_node == (tree->leftmost + 1)) {

                    --(*current_node);

                    return;

                }else if(!(*current_node) && !tree->is_empty()) {

                    *current_node = tree->leftmost + 1;

                    return;

                }

                if((*current_node)->left) {

                    if((*current_node)->left->right) {

                        *current_node = (*current_node)->left->right.get();

                        while((*current_node)->right) *current_node = (*current_node)->right.get();

                    }else {

                        *current_node = (*current_node)->left.get();

                    }

                }else if((*current_node)->parent) {

                    while(*current_node == (*current_node)->parent->left.get()) *current_node = (*current_node)->parent;

                    *current_node = (*current_node)->parent;

                }
            }
        };

        class IteratorBase : protected BidirectionalImpl
        {
        public:
            using value_type = Node;
            using reference  = value_type&;
            using pointer    = value_type*;

            using iterator_category = std::bidirectional_iterator_tag;
            using difference_type   = std::ptrdiff_t;

            constexpr bool operator==(const IteratorBase& other) const noexcept
            {
                return current_node == other.current_node;
            }

            constexpr bool operator!=(const IteratorBase& other) const noexcept
            {
                return current_node != other.current_node;
            }

        protected:
            constexpr IteratorBase(std::nullptr_t = nullptr) noexcept : current_node{}, tree{} {}

            constexpr explicit IteratorBase(const BinarySearchTree* tree_) noexcept : current_node{}, tree{tree_} {}

            constexpr IteratorBase(pointer node, const BinarySearchTree* tree_) noexcept : current_node{node}, tree{tree_} {}

            IteratorBase(const IteratorBase&) = default;

            constexpr IteratorBase(IteratorBase&& ib) noexcept : current_node{ib.current_node}, tree{ib.tree}
            {
                ib.current_node = nullptr;
            }

            ~IteratorBase() noexcept {}

            IteratorBase& operator=(const IteratorBase&) = default;

            constexpr IteratorBase& operator=(IteratorBase&& ib) noexcept
            {
                if(&ib == this) return *this;

                current_node = ib.current_node;
                tree         = ib.tree;

                ib.current_node = nullptr;

                return *this;
            }

            pointer current_node;

            const BinarySearchTree* tree;
        };

        class Iterator final : public IteratorBase
        {
        public:
            constexpr Iterator(std::nullptr_t = nullptr) noexcept : IteratorBase{} {}

            Iterator& operator++()
            {
                BidirectionalImpl::next(&(this->current_node), this->tree);

                return *this;
            }

            Iterator operator++(int)
            {
                Iterator old(*this);

                BidirectionalImpl::next(&(this->current_node), this->tree);

                return old;
            }

            Iterator& operator--()
            {
                BidirectionalImpl::previous(&(this->current_node), this->tree);

                return *this;
            }

            Iterator operator--(int)
            {
                Iterator old(*this);

                BidirectionalImpl::previous(&(this->current_node), this->tree);

                return old;
            }

            constexpr typename IteratorBase::reference operator*()
            {
                if(!this->current_node && !this->tree->is_empty()) this->current_node = this->tree->rightmost - 1;

                return *this->current_node;
            }

            constexpr typename IteratorBase::pointer operator->() noexcept
            {
                if(!this->current_node && !this->tree->is_empty()) this->current_node = this->tree->rightmost - 1;

                return this->current_node;
            }

        private:
            friend class BinarySearchTree;
            friend class ConstIteratorBase;

            constexpr explicit Iterator(const BinarySearchTree* tree_) noexcept : IteratorBase(tree_) {}

            constexpr Iterator(typename IteratorBase::pointer node, const BinarySearchTree* tree_) noexcept : IteratorBase(node, tree_) {}
        };

        class ReverseIterator final : public IteratorBase
        {
        public:
            constexpr ReverseIterator(std::nullptr_t = nullptr) noexcept : IteratorBase{} {}

            ReverseIterator& operator++()
            {
                BidirectionalImpl::previous(&(this->current_node), this->tree);

                return *this;
            }

            ReverseIterator operator++(int)
            {
                ReverseIterator old(*this);

                BidirectionalImpl::previous(&(this->current_node), this->tree);

                return old;
            }

            ReverseIterator& operator--()
            {
                BidirectionalImpl::next(&(this->current_node), this->tree);

                return *this;
            }

            ReverseIterator operator--(int)
            {
                ReverseIterator old(*this);

                BidirectionalImpl::next(&(this->current_node), this->tree);

                return old;
            }

            constexpr typename IteratorBase::reference operator*()
            {
                if(!this->current_node && !this->tree->is_empty()) this->current_node = this->tree->leftmost + 1;

                return *this->current_node;
            }

            constexpr typename IteratorBase::pointer operator->() noexcept
            {
                if(!this->current_node && !this->tree->is_empty()) this->current_node = this->tree->leftmost + 1;

                return this->current_node;
            }

        private:
            friend class BinarySearchTree;

            constexpr explicit ReverseIterator(const BinarySearchTree* tree_) noexcept : IteratorBase(tree_) {}

            constexpr ReverseIterator(typename IteratorBase::pointer node, const BinarySearchTree* tree_) noexcept : IteratorBase(node, tree_) {}
        };

        class ConstIteratorBase : protected BidirectionalImpl
        {
        public:
            using value_type = Node;
            using reference  = const value_type&;
            using pointer    = const value_type*;

            using iterator   = typename BinarySearchTree::Iterator;

            using iterator_category = std::bidirectional_iterator_tag;
            using difference_type   = std::ptrdiff_t;

            constexpr bool operator==(const ConstIteratorBase& other) const noexcept
            {
                return current_node == other.current_node;
            }

            constexpr bool operator!=(const ConstIteratorBase& other) const noexcept
            {
                return current_node != other.current_node;
            }

        protected:
            constexpr ConstIteratorBase(std::nullptr_t = nullptr) noexcept : current_node{}, tree{} {}

            constexpr ConstIteratorBase(const iterator& it) noexcept : current_node{it.current_node}, tree{it.tree} {}

            constexpr explicit ConstIteratorBase(const BinarySearchTree* tree_) noexcept : current_node{}, tree{tree_} {}

            constexpr ConstIteratorBase(pointer node, const BinarySearchTree* tree_) noexcept : current_node{node}, tree{tree_} {}

            ConstIteratorBase(const ConstIteratorBase&) = default;

            constexpr ConstIteratorBase(ConstIteratorBase&& cib) noexcept : current_node{cib.current_node}, tree{cib.tree}
            {
                cib.current_node = nullptr;
            }

            ~ConstIteratorBase() noexcept {}

            ConstIteratorBase& operator=(const ConstIteratorBase&) = default;

            constexpr ConstIteratorBase& operator=(ConstIteratorBase&& cib) noexcept
            {
                if(&cib == this) return *this;

                current_node = cib.current_node;
                tree         = cib.tree;

                cib.current_node = nullptr;

                return *this;
            }

            pointer current_node;

            const BinarySearchTree* tree;
        };

        class ConstIterator final : public ConstIteratorBase
        {
        public:
            constexpr ConstIterator(std::nullptr_t = nullptr) noexcept : ConstIteratorBase{} {}

            constexpr ConstIterator(const typename ConstIteratorBase::iterator& it) noexcept : ConstIteratorBase(it) {}

            ConstIterator& operator++()
            {
                BidirectionalImpl::next(&(this->current_node), this->tree);

                return *this;
            }

            ConstIterator operator++(int)
            {
                ConstIterator old(*this);

                BidirectionalImpl::next(&(this->current_node), this->tree);

                return old;
            }

            ConstIterator& operator--()
            {
                BidirectionalImpl::previous(&(this->current_node), this->tree);

                return *this;
            }

            ConstIterator operator--(int)
            {
                ConstIterator old(*this);

                BidirectionalImpl::previous(&(this->current_node), this->tree);

                return old;
            }

            constexpr typename ConstIteratorBase::reference operator*()
            {
                if(!this->current_node && !this->tree->is_empty()) this->current_node = this->tree->rightmost - 1;

                return *this->current_node;
            }

            constexpr typename ConstIteratorBase::pointer operator->() noexcept
            {
                if(!this->current_node && !this->tree->is_empty()) this->current_node = this->tree->rightmost - 1;

                return this->current_node;
            }

        private:
            friend class BinarySearchTree;

            constexpr explicit ConstIterator(const BinarySearchTree* tree_) noexcept : ConstIteratorBase(tree_) {}

            constexpr ConstIterator(typename ConstIteratorBase::pointer node, const BinarySearchTree* tree_) noexcept : ConstIteratorBase(node, tree_) {}
        };

        class ConstReverseIterator final : public ConstIteratorBase
        {
        public:
            constexpr ConstReverseIterator(std::nullptr_t = nullptr) noexcept : IteratorBase{} {}

            ConstReverseIterator& operator++()
            {
                BidirectionalImpl::previous(&(this->current_node), this->tree);

                return *this;
            }

            ConstReverseIterator operator++(int)
            {
                ConstReverseIterator old(*this);

                BidirectionalImpl::previous(&(this->current_node), this->tree);

                return old;
            }

            ConstReverseIterator& operator--()
            {
                BidirectionalImpl::next(&(this->current_node), this->tree);

                return *this;
            }

            ConstReverseIterator operator--(int)
            {
                ConstReverseIterator old(*this);

                BidirectionalImpl::next(&(this->current_node), this->tree);

                return old;
            }

            constexpr typename ConstIteratorBase::reference operator*()
            {
                if(!this->current_node && !this->tree->is_empty()) this->current_node = this->tree->leftmost + 1;

                return *this->current_node;
            }

            constexpr typename ConstIteratorBase::pointer operator->() noexcept
            {
                if(!this->current_node && !this->tree->is_empty()) this->current_node = this->tree->leftmost + 1;

                return this->current_node;
            }

        private:
            friend class BinarySearchTree;

            constexpr explicit ConstReverseIterator(const BinarySearchTree* tree_) noexcept : IteratorBase(tree_) {}

            constexpr ConstReverseIterator(typename ConstIteratorBase::pointer node, const BinarySearchTree* tree_) noexcept : IteratorBase(node, tree_) {}
        };

        constexpr explicit BinarySearchTree(const CompareType& comp_ = CompareType{}) : sz{}, comp(comp_), rightmost{}, leftmost{}, root{} {}

        template<typename InputIterator>
        BinarySearchTree(InputIterator it1, InputIterator it2, const CompareType& comp_ = CompareType{}) : sz{}, comp(comp_), rightmost{}, leftmost{}, root{}
        {
            add(it1, it2);
        }

        BinarySearchTree(std::initializer_list<KeyType> il, const CompareType& comp_ = CompareType{}) : BinarySearchTree(il.begin(), il.end(), comp_)
        {
            //
        }

        BinarySearchTree(std::initializer_list<KeyValueType> il, const CompareType& comp_ = CompareType{}) : BinarySearchTree(il.begin(), il.end(), comp_)
        {
            //
        }

        BinarySearchTree(const BinarySearchTree& bst) :
            sz{bst.sz},
            comp(bst.comp),
            rightmost{},
            leftmost{},
            root{}
        {
            add(bst.cbegin(), bst.cend());
        }

        BinarySearchTree(BinarySearchTree&& bst) :
            sz{bst.sz},
            comp(std::move(bst.comp)),
            rightmost{bst.rightmost},
            leftmost{bst.leftmost},
            root(std::move(bst.root))
        {
            bst.sz = 0;

            bst.rightmost = bst.leftmost = nullptr;
        }

        ~BinarySearchTree()
        {
            delete_tree();
        }

        BinarySearchTree& operator=(const BinarySearchTree& bst)
        {
            if(&bst == this) return *this;

            delete_tree();

            sz   = bst.sz;
            comp = bst.comp;

            add(bst.cbegin(), bst.cend());

            return *this;
        }

        BinarySearchTree& operator=(BinarySearchTree&& bst)
        {
            if(&bst == this) return *this;

            delete_tree();

            sz        = bst.sz;
            comp      = std::move(bst.comp);
            rightmost = bst.rightmost;
            leftmost  = bst.leftmost;
            root      = std::move(bst.root);

            bst.sz = 0;

            bst.rightmost = bst.leftmost = nullptr;

            return *this;
        }

        [[nodiscard]] std::optional<ValueType>& at(const KeyType& key)
        {
            return walk(key)->value;
        }

        [[nodiscard]] const std::optional<ValueType>& at(const KeyType& key) const
        {
            return walk(key)->value;
        }

        [[nodiscard]] std::optional<ValueType>& operator[](const KeyType& key)
        {
            return walk_n_insert(key, std::optional<ValueType>{})->value;
        }

        [[nodiscard]] Iterator begin() noexcept { return root ? Iterator(leftmost + 1, this) : Iterator(this); }

        [[nodiscard]] ConstIterator begin() const noexcept { return root ? ConstIterator(leftmost + 1, this) : ConstIterator(this); }

        [[nodiscard]] ConstIterator cbegin() const noexcept { return root ? ConstIterator(leftmost + 1, this) : ConstIterator(this); }

        [[nodiscard]] ReverseIterator rbegin() noexcept { return root ? ReverseIterator(rightmost - 1, this) : ReverseIterator(this); }

        [[nodiscard]] ConstReverseIterator rbegin() const noexcept { return root ? ConstReverseIterator(rightmost - 1, this) : ConstReverseIterator(this); }

        [[nodiscard]] ConstReverseIterator crbegin() const noexcept { return root ? ConstReverseIterator(rightmost - 1, this) : ConstReverseIterator(this); }

        [[nodiscard]] Iterator end() noexcept { return Iterator(rightmost, this); }

        [[nodiscard]] ConstIterator end() const noexcept { return ConstIterator(rightmost, this); }

        [[nodiscard]] ConstIterator cend() const noexcept { return ConstIterator(rightmost, this); }

        [[nodiscard]] ReverseIterator rend() noexcept { return ReverseIterator(leftmost, this); }

        [[nodiscard]] ConstReverseIterator rend() const noexcept { return ConstReverseIterator(leftmost, this); }

        [[nodiscard]] ConstReverseIterator crend() const noexcept { return ConstReverseIterator(leftmost, this); }

        [[nodiscard]] constexpr bool is_empty() const noexcept { return sz == 0; }

        [[nodiscard]] constexpr size_t get_size() const noexcept { return sz; }

        [[nodiscard]] constexpr const CompareType& get_comp() const noexcept { return comp; }

        void clear()
        {
            delete_tree();
        }

        void add(const Node& node)
        {
            walk_n_insert(node.key, node.value);
        }

        void add(const KeyType& key)
        {
            walk_n_insert(key, std::optional<ValueType>{});
        }

        void add(KeyType&& key)
        {
            walk_n_insert(std::move(key), std::optional<ValueType>{});
        }

        void add(const KeyValueType& kv)
        {
            const auto& [key, value] = kv;

            walk_n_insert(key, value);
        }

        void add(KeyValueType&& kv)
        {
            auto& [key, value] = kv;

            walk_n_insert(std::move(key), std::move(value));
        }

        template<typename InputIterator>
        void add(InputIterator it1, InputIterator it2)
        {
            for(; it1 != it2; ++it1) add(*it1);
        }

        void add(std::initializer_list<KeyType> il)
        {
            add(il.begin(), il.end());
        }

        void add(std::initializer_list<KeyValueType> il)
        {
            add(il.begin(), il.end());
        }

        template<typename K>
        [[nodiscard]] bool contains(const K& x) const noexcept
        {
            try {

                walk(x);

            }catch(const std::out_of_range&) {

                return false;

            }

            return true;
        }

        template<typename K>
        [[nodiscard]] Iterator find(const K& x)
        {
            auto it = begin();

            for(; it != end(); ++it) {

                if(it->get_key() == x) break;

            }

            return it;
        }

        template<typename K>
        [[nodiscard]] ConstIterator find(const K& x) const
        {
            auto cit = cbegin();

            for(; cit != cend(); ++cit) {

                if(cit->get_key() == x) break;

            }

            return cit;
        }

        template<typename K>
        [[nodiscard]] std::pair<Iterator, Iterator> equal_range(const K& x)
        {
            auto it1 = begin(), it2 = begin();

            for(;(it1 != end()) && (it2 != end());) {

                if(it2->get_key() > x) {

                    break;

                }else if(it1->get_key() >= x) {

                    ++it2;

                }else {

                    ++it1;
                    ++it2;

                }

            }

            return std::pair<Iterator, Iterator>(it1, it2);
        }

        template<typename K>
        [[nodiscard]] std::pair<ConstIterator, ConstIterator> equal_range(const K& x) const
        {
            auto cit1 = cbegin(), cit2 = cbegin();

            for(;(cit1 != cend()) && (cit2 != cend());) {

                if(cit2->get_key() > x) {

                    break;

                }else if(cit1->get_key() >= x) {

                    ++cit2;

                }else {

                    ++cit1;
                    ++cit2;

                }

            }

            return std::pair<ConstIterator, ConstIterator>(cit1, cit2);
        }

        template<typename K>
        [[nodiscard]] Iterator lower_bound(const K& x)
        {
            auto it = begin();

            for(; it != end(); ++it) {

                if(it->get_key() >= x) break;

            }

            return it;
        }

        template<typename K>
        [[nodiscard]] ConstIterator lower_bound(const K& x) const
        {
            auto cit = cbegin();

            for(; cit != cend(); ++cit) {

                if(cit->get_key() >= x) break;

            }

            return cit;
        }

        template<typename K>
        [[nodiscard]] Iterator upper_bound(const K& x)
        {
            auto it = begin();

            for(; it != end(); ++it) {

                if(it->get_key() > x) break;

            }

            return it;
        }

        template<typename K>
        [[nodiscard]] ConstIterator upper_bound(const K& x) const
        {
            auto cit = cbegin();

            for(; cit != cend(); ++cit) {

                if(cit->get_key() > x) break;

            }

            return cit;
        }

        void swap(BinarySearchTree& other) noexcept(std::is_nothrow_swappable_v<CompareType>)
        {
            auto temp = std::move(other);

            other = std::move(*this);

            *this = std::move(temp);
        }

        template<typename T, typename U>
        [[nodiscard]] constexpr KeyValueType make_kv(T&& t, U&& u) const
        {
            return KeyValueType(std::forward<T>(t), std::forward<U>(u));
        }

    private:
        void delete_tree()
        {
            if(!root) return;

            std::stack next_nodes(std::vector<std::unique_ptr<Node>>{});

            next_nodes.emplace(std::move(root));

            while(!next_nodes.empty()) {

                const auto current_node = std::move(next_nodes.top());

                next_nodes.pop();

                if(current_node->left) next_nodes.emplace(std::move(current_node->left));

                if(current_node->right) next_nodes.emplace(std::move(current_node->right));

            }

            sz = 0;

            rightmost = leftmost = nullptr;
        }

        size_t height(Node* node) const
        {
            if(!node) return 0;

            std::queue<Node*> next_nodes{};

            size_t result{}, node_count{};

            Node* current_node{};

            next_nodes.push(node);

            while(!next_nodes.empty()) {

                ++result;

                node_count = next_nodes.size();

                while(node_count--) {

                    current_node = next_nodes.front();

                    if(current_node->left)  next_nodes.push(current_node->left.get());

                    if(current_node->right) next_nodes.push(current_node->right.get());

                    next_nodes.pop();

                }

            }

            return result - 1;
        }

        bool is_balanced(Node* node) const
        {
            if(!node) return false;

            int diff = height(node->right.get()) - height(node->left.get());

            return (diff >= -1) && (diff <= 1);
        }

        void rotate_left(Node* parent) noexcept
        {
            Node* right       = parent->right.get();
            Node* right_left  = right->left.get();
            Node* grandparent = parent->parent;

            parent->right.release();

            parent->right.reset(right_left);

            if(right_left) right_left->parent = parent;

            right->left.release();

            right->left.reset(parent);

            parent->parent = right;

            right->parent = grandparent;

            if(!grandparent) {

                root.release();

                root.reset(right);

            }else {

                std::unique_ptr<Node>* sub_tree{};

                if(parent == grandparent->left.get()) sub_tree = &grandparent->left;
                else                                  sub_tree = &grandparent->right;

                sub_tree->release();

                sub_tree->reset(right);

            }

            parent->balance_factor = right->balance_factor = 0;
        }

        void rotate_right(Node* parent) noexcept
        {
            Node* left        = parent->left.get();
            Node* left_right  = left->right.get();
            Node* grandparent = parent->parent;

            parent->left.release();

            parent->left.reset(left_right);

            if(left_right) left_right->parent = parent;

            left->right.release();

            left->right.reset(parent);

            parent->parent = left;

            left->parent = grandparent;

            if(!grandparent) {

                root.release();

                root.reset(left);

            }else {

                std::unique_ptr<Node>* sub_tree{};

                if(parent == grandparent->left.get()) sub_tree = &grandparent->left;
                else                                  sub_tree = &grandparent->right;

                sub_tree->release();

                sub_tree->reset(left);

            }

            parent->balance_factor = left->balance_factor = 0;
        }

        void rotate_left_right(Node* parent) noexcept
        {
            Node* left       = parent->left.get();
            Node* left_right = left->right.get();

            rotate_left(parent->left.get());

            rotate_right(parent);

            switch(left_right->balance_factor) {

                case 1: {

                    parent->balance_factor =  0;
                    left->balance_factor   = -1;

                } break;

                case -1: {

                    parent->balance_factor = 1;
                    left->balance_factor   = 0;

                } break;

                default: {

                    parent->balance_factor = left->balance_factor = 0;

                } break;

            }
        }

        void rotate_right_left(Node* parent) noexcept
        {
            Node* right      = parent->right.get();
            Node* right_left = right->left.get();

            rotate_right(parent->right.get());

            rotate_left(parent);

            switch(right_left->balance_factor) {

                case 1: {

                    parent->balance_factor =  0;
                    right->balance_factor  = -1;

                } break;

                case -1: {

                    parent->balance_factor = 1;
                    right->balance_factor  = 0;

                } break;

                default: {

                    parent->balance_factor = right->balance_factor = 0;

                } break;

            }
        }

        void balance(Node* parent, Node* curr_node) noexcept
        {
            while(parent) {

                parent->balance_factor += (curr_node == parent->left.get() ? -1 : 1);

                if(parent->balance_factor == 0) {

                    break;

                }else if((parent->balance_factor == -1) || (parent->balance_factor == 1)) {

                    curr_node = parent;

                    parent = curr_node->parent;

                }else {

                    if(parent->balance_factor == 2) {

                        if(curr_node->balance_factor == 1) rotate_left(parent);
                        else                               rotate_right_left(parent);

                    }else if(parent->balance_factor == -2) {

                        if(curr_node->balance_factor == -1) rotate_right(parent);
                        else                                rotate_left_right(parent);

                    }

                    break;

                }

            }
        }

        template<typename K>
        Node* walk(const K& x) const
        {
            if(!root) throw std::out_of_range("Key doesn't exist");

            Node* curr_node = root.get();

            while(curr_node) {

                if(comp(curr_node->key, x)) {

                    curr_node = curr_node->right.get();

                }else if(curr_node->key == x) {

                    return curr_node;

                }else {

                    curr_node = curr_node->left.get();

                }

            }

            throw std::out_of_range("Key doesn't exist");
        }

        template<typename T, typename U>
        Node* walk_n_insert(T&& key, U&& value)
        {
            if(!root) {

                root = std::make_unique<Node>(nullptr, std::forward<T>(key), std::forward<U>(value));

                ++sz;

                leftmost  = root.get() - 1;
                rightmost = leftmost   + 2;

                return root.get();

            }

            Node* curr_root = root.get();

            Node* new_node{};

            bool is_first{true}, is_last{true};

            while(true) {

                if(comp(curr_root->key, key)) {

                    is_first = false;

                    if(!curr_root->right) {

                        curr_root->right = std::make_unique<Node>(curr_root, std::forward<T>(key), std::forward<U>(value));

                        new_node = curr_root->right.get();

                        ++sz;

                        if(is_last) rightmost = curr_root->right.get() + 1;

                        balance(curr_root, curr_root->right.get());

                        break;

                    }

                    curr_root = curr_root->right.get();

                }else if(curr_root->key == key) {

                    new_node = curr_root;

                    break;

                }else {

                    is_last = false;

                    if(!curr_root->left) {

                        curr_root->left = std::make_unique<Node>(curr_root, std::forward<T>(key), std::forward<U>(value));

                        new_node = curr_root->left.get();

                        ++sz;

                        if(is_first) leftmost = curr_root->left.get() - 1;

                        balance(curr_root, curr_root->left.get());

                        break;

                    }

                    curr_root = curr_root->left.get();

                }

            }

            return new_node;
        }

        friend bool operator==(const BinarySearchTree<Key, Compare, Value>& lhs, const BinarySearchTree<Key, Compare, Value>& rhs)
        {
            if(lhs.sz != rhs.sz) return false;

            auto lhs_it = lhs.cbegin();
            auto rhs_it = rhs.cbegin();

            for(; lhs_it != lhs.cend(); ++lhs_it, ++rhs_it) {

                if(*lhs_it != *rhs_it) return false;

            }

            return true;
        }

        friend bool operator!=(const BinarySearchTree<Key, Compare, Value>& lhs, const BinarySearchTree<Key, Compare, Value>& rhs)
        {
            return !operator==(lhs, rhs);
        }

        friend bool operator<(const BinarySearchTree<Key, Compare, Value>& lhs, const BinarySearchTree<Key, Compare, Value>& rhs)
        {
            return std::lexicographical_compare(lhs.cbegin(), lhs.cend(), rhs.cbegin(), rhs.cend());
        }

        friend bool operator<=(const BinarySearchTree<Key, Compare, Value>& lhs, const BinarySearchTree<Key, Compare, Value>& rhs)
        {
            return std::lexicographical_compare(lhs.cbegin(), lhs.cend(), rhs.cbegin(), rhs.cend(), std::less_equal<typename BinarySearchTree<Key, Compare, Value>::Node>{});
        }

        friend bool operator>(const BinarySearchTree<Key, Compare, Value>& lhs, const BinarySearchTree<Key, Compare, Value>& rhs)
        {
            return std::lexicographical_compare(lhs.cbegin(), lhs.cend(), rhs.cbegin(), rhs.cend(), std::greater<typename BinarySearchTree<Key, Compare, Value>::Node>{});
        }

        friend bool operator>=(const BinarySearchTree<Key, Compare, Value>& lhs, const BinarySearchTree<Key, Compare, Value>& rhs)
        {
            return std::lexicographical_compare(lhs.cbegin(), lhs.cend(), rhs.cbegin(), rhs.cend(), std::greater_equal<typename BinarySearchTree<Key, Compare, Value>::Node>{});
        }

        size_t sz;

        CompareType comp;

        Node* rightmost, *leftmost; // these two point to both the +1 and -1 addresses of the rightmost and leftmost nodes respectively

        std::unique_ptr<Node> root;
    };

    template<typename Key, typename Value = Key, typename Compare = std::less<Key>, typename... TArgs>
    [[nodiscard]] auto make_bst(TArgs&&... args)
    {
        return BinarySearchTree<Key, Compare, Value>(std::forward<TArgs>(args)...);
    }

    template<typename Key, typename Value = Key, typename... TArgs>
    [[nodiscard]] auto make_asc_bst(TArgs&&... args)
    {
        return BinarySearchTree<Key, std::less<Key>, Value>(std::forward<TArgs>(args)...);
    }

    template<typename Key, typename Value = Key, typename... TArgs>
    [[nodiscard]] auto make_desc_bst(TArgs&&... args)
    {
        return BinarySearchTree<Key, std::greater<Key>, Value>(std::forward<TArgs>(args)...);
    }

    template<typename Key>
    [[nodiscard]] auto make_asc_bst(std::initializer_list<Key> keys)
    {
        return BinarySearchTree<Key, std::less<Key>>(keys);
    }

    template<typename Key, typename Value = Key>
    [[nodiscard]] auto make_asc_bst(std::initializer_list<typename BinarySearchTree<Key, std::less<Key>, Value>::KeyValueType> kvs)
    {
        return BinarySearchTree<Key, std::less<Key>, Value>(kvs);
    }

    template<typename Key>
    [[nodiscard]] auto make_desc_bst(std::initializer_list<Key> keys)
    {
        return BinarySearchTree<Key, std::greater<Key>>(keys);
    }

    template<typename Key, typename Value = Key>
    [[nodiscard]] auto make_desc_bst(std::initializer_list<typename BinarySearchTree<Key, std::greater<Key>, Value>::KeyValueType> kvs)
    {
        return BinarySearchTree<Key, std::greater<Key>, Value>(kvs);
    }

    template<typename InputIterator, typename Compare = std::less<typename std::iterator_traits<InputIterator>::value_type::first_type>>
    BinarySearchTree(InputIterator, InputIterator, const Compare& = Compare{})
    ->
    BinarySearchTree<typename std::iterator_traits<InputIterator>::value_type::first_type, Compare, typename std::iterator_traits<InputIterator>::value_type::second_type>;

    template<typename KeyValue, typename Compare = std::less<typename KeyValue::first_type>>
    BinarySearchTree(std::initializer_list<KeyValue>, const Compare& = Compare{})
    ->
    BinarySearchTree<typename KeyValue::first_type, Compare, typename KeyValue::second_type>;

}

namespace std {

    template<typename Key, typename Compare, typename Value>
    void swap(containers::trees::BinarySearchTree<Key, Compare, Value>& lhs, containers::trees::BinarySearchTree<Key, Compare, Value>& rhs) noexcept(noexcept(lhs.swap(rhs)))
    {
        lhs.swap(rhs);
    }

}

#endif // BINARY_SEARCH_TREE_H

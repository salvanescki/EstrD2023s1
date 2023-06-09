#include <iostream>
#include "Tree.h"

using namespace std;

struct NodeT {
    int elem;
    NodeT* left;
    NodeT* right;
};

Tree emptyT(){
    return NULL;
}

Tree nodeT(int elem, Tree left, Tree right){
    NodeT* t = new NodeT;
    t->elem = elem;
    t->left = left;
    t->right = right;
    return t;
}

bool isEmptyT(Tree t){
    return t==NULL;
}

int rootT(Tree t){
    return t->elem;
}

Tree left(Tree t){
    return t->left;
}

Tree right(Tree t){
    return t->right;
}
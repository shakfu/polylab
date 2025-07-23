class UniqueList(list):
    """
    A list subclass that maintains uniqueness of elements and provides additional utility methods.
    """
    
    def __init__(self, iterable=None):
        """Initialize UniqueList, ensuring all elements are unique."""
        super().__init__()
        if iterable is not None:
            for item in iterable:
                self.add(item)
    
    def add(self, item):
        """Add an item only if it's not already in the list."""
        if item not in self:
            self.append(item)
        return self
    
    def append(self, item):
        """Override append to maintain uniqueness."""
        if item not in self:
            super().append(item)
    
    def extend(self, iterable):
        """Override extend to maintain uniqueness."""
        for item in iterable:
            self.add(item)
    
    def insert(self, index, item):
        """Override insert to maintain uniqueness."""
        if item not in self:
            super().insert(index, item)
    
    def __iadd__(self, other):
        """Override += operator to maintain uniqueness."""
        self.extend(other)
        return self
    
    def __add__(self, other):
        """Override + operator to return a new UniqueList."""
        result = UniqueList(self)
        result.extend(other)
        return result
    
    def union(self, other):
        """Return a new UniqueList with elements from both lists."""
        return self + other
    
    def intersection(self, other):
        """Return a new UniqueList with elements common to both lists."""
        return UniqueList(item for item in self if item in other)
    
    def difference(self, other):
        """Return a new UniqueList with elements in self but not in other."""
        return UniqueList(item for item in self if item not in other)
    
    def symmetric_difference(self, other):
        """Return a new UniqueList with elements in either list but not both."""
        return UniqueList(
            list(self.difference(other)) + list(UniqueList(other).difference(self))
        )
    
    def toggle(self, item):
        """Add item if not present, remove if present."""
        if item in self:
            self.remove(item)
        else:
            self.add(item)
        return self
    
    def first(self, default=None):
        """Return the first element or default if empty."""
        return self[0] if self else default
    
    def last(self, default=None):
        """Return the last element or default if empty."""
        return self[-1] if self else default
    
    def unique_count(self):
        """Return the count of unique elements (same as len for this class)."""
        return len(self)
    
    def __repr__(self):
        """Custom representation showing it's a UniqueList."""
        return f"UniqueList({super().__repr__()})"


# Example usage and demonstration
if __name__ == "__main__":
    # Create a UniqueList
    ul = UniqueList([1, 2, 3, 2, 4, 1, 5])
    print(f"Created from [1, 2, 3, 2, 4, 1, 5]: {ul}")
    
    # Add elements
    ul.add(6)
    ul.add(3)  # Won't be added again
    print(f"After adding 6 and 3: {ul}")
    
    # Use append (maintains uniqueness)
    ul.append(7)
    ul.append(1)  # Won't be added again
    print(f"After appending 7 and 1: {ul}")
    
    # Set operations
    ul2 = UniqueList([4, 5, 6, 7, 8])
    print(f"Second list: {ul2}")
    print(f"Union: {ul.union(ul2)}")
    print(f"Intersection: {ul.intersection(ul2)}")
    print(f"Difference: {ul.difference(ul2)}")
    print(f"Symmetric difference: {ul.symmetric_difference(ul2)}")
    
    # Toggle functionality
    ul.toggle(9)  # Add 9
    ul.toggle(5)  # Remove 5
    print(f"After toggling 9 and 5: {ul}")
    
    # First and last elements
    print(f"First element: {ul.first()}")
    print(f"Last element: {ul.last()}")
    
    # Works with standard list operations
    print(f"Length: {len(ul)}")
    print(f"Contains 3: {3 in ul}")
    print(f"Index of 4: {ul.index(4)}")
    
    # Slicing works too
    print(f"First 3 elements: {ul[:3]}")

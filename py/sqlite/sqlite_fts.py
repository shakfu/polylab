"""sqlite_fts.py

code from 

"Using SQLite FTS (Full-Text Search) with Python"

from: https://medium.com/@ccpythonprogramming/using-sqlite-fts-full-text-search-with-python-5d749ea29859

Full-text search (FTS) is a powerful feature that allows you to search text-heavy
databases quickly. With SQLite, you can use FTS to search large bodies of text more
efficiently than standard queries. This is particularly useful when you have datasets
like articles, product descriptions, or any other textual data that needs to be
searched frequently.

In this article, we’ll explore how to enable FTS in SQLite using Python,
create a dummy database, and demonstrate some search queries.
"""

import sqlite3

# Connect to the database (creates one if it doesn't exist)
conn = sqlite3.connect('articles.db')
cursor = conn.cursor()

# Create an FTS5 virtual table for articles
cursor.execute('''
    CREATE VIRTUAL TABLE IF NOT EXISTS articles USING fts5(title, content)
''')

# Insert some dummy data into the articles table
articles = [
    ("Python Full-Text Search", "Learn how to enable full-text search in SQLite using Python."),
    ("Introduction to SQLite", "SQLite is a lightweight database engine. It’s great for local storage."),
    ("Working with Databases", "Database management is key for application development."),
    ("Advanced SQL Techniques", "Mastering SQL queries can significantly improve your application performance.")
]

cursor.executemany('INSERT INTO articles (title, content) VALUES (?, ?)', articles)

# Commit changes to the database
conn.commit()

# Search for articles that mention 'Python'
query = "SELECT title, content FROM articles WHERE articles MATCH 'Python'"
cursor.execute(query)

# Fetch and print the results
results = cursor.fetchall()
print("\n--- Articles that mention 'Python' ---")
for title, content in results:
    print(f"Title: {title}\nContent: {content}\n")

# Search for articles mentioning 'Python' or 'SQLite'
query = "SELECT title, content FROM articles WHERE articles MATCH 'Python OR SQLite'"
cursor.execute(query)

# Fetch and print the results
results = cursor.fetchall()
print("\n--- Articles that mention 'Python' or 'SQLite' ---")
for title, content in results:
    print(f"Title: {title}\nContent: {content}\n")

# Search for an exact phrase 'database management'
query = "SELECT title, content FROM articles WHERE articles MATCH '\"database management\"'"
cursor.execute(query)

# Fetch and print the results
results = cursor.fetchall()
print("\n--- Articles that mention the exact phrase 'database management' ---")
for title, content in results:
    print(f"Title: {title}\nContent: {content}\n")

# Prefix search for words starting with 'adv'
query = "SELECT title, content FROM articles WHERE articles MATCH 'adv*'"
cursor.execute(query)

# Fetch and print the results
results = cursor.fetchall()
print("\n--- Articles with words starting with 'adv' ---")
for title, content in results:
    print(f"Title: {title}\nContent: {content}\n")

# Limit the number of results to 2
query = "SELECT title, content FROM articles WHERE articles MATCH 'SQL' LIMIT 2"
cursor.execute(query)

# Fetch and print the limited results
results = cursor.fetchall()
print("\n--- Limited search results for 'SQL' (max 2) ---")
for title, content in results:
    print(f"Title: {title}\nContent: {content}\n")

# Close the connection
conn.close()

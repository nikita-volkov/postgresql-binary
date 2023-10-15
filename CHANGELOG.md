# 0.13.1.1

- GHC 9.8 portability

# 0.13

- Removed `PostgreSQL.Binary.Data`. Because it was causing Haddock in the dependant packages to unpredictably redirect to it instead of the original location regardless of whether they were imported from it.

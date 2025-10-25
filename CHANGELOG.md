# 0.15

## Breaking Changes

- Made the Composite decoder check for exact field count match
- Dropped the Monad and MonadFail instances for it

# 0.14

- Moved to "iproute" from "network-ip" for inet datatypes

# 0.13

- Removed `PostgreSQL.Binary.Data`. Because it was causing Haddock in the dependant packages to unpredictably redirect to it instead of the original location regardless of whether they were imported from it.

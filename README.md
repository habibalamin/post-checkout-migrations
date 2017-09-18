# post-checkout-migrations - remember to roll back migrations

post-checkout-migrations is a git hook that saves removed migrations into a temporary directory after a checkout and reminds you to roll them back to keep your database in sync with your branches.

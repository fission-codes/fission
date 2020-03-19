ALTER TABLE user_cid  RENAME TO loose_pin;
ALTER TABLE loose_pin RENAME COLUMN     user_fk TO owner_id;
ALTER TABLE loose_pin DROP   COLUMN     modified_at;

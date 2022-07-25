UPDATE "user" SET public_key = (
  CASE

  -- Already migrated?
  WHEN starts_with(public_key, '{') THEN
    public_key

  -- Before this migration was created we only had ED25519 and RSA keys
  WHEN starts_with(public_key, 'MII') THEN
    format('{"key":"%s","type":"%s"}', public_key, 'RSAPublicKey')

  ELSE
    format('{"key":"%s","type":"%s"}', public_key, 'Ed25519PublicKey')

  END
)
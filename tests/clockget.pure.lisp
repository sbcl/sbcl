#+linux ; the CLOCK- symbols may not exist
(with-test (:name :clock-gettime)
  (sb-unix:clock-gettime sb-unix:clock-monotonic-raw)
  (sb-unix:clock-gettime sb-unix:clock-monotonic-coarse)
  (sb-unix:clock-gettime sb-unix:clock-monotonic))

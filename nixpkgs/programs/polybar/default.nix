{
  services.polybar = {
    enable = true;
    script = "polybar mainbar &";
    config = {
      "bar/mainbar" = {
        width="100%";
        height=200;
        "offset-y"=20;
        background="#000";
      };
    };
  };
}

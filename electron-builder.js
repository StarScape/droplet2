let config = {
  appId: "com.jackarrington.droplet",
  productName: "Droplet",
  copyright: "Copyright © 2022 Jack Arrington",
  directories: {
    app: ".",
    output: "dist",
    buildResources: "build_res"
  },
  files: [
    "package.json",
    "public/**/*",
    "node_modules"
  ],
  dmg: {
    background: null,
    backgroundColor: "#ffffff",
    window: {
      width: "400",
      height: "300"
    },
    contents: [
      {
        x: 100,
        y: 100
      },
      {
        x: 300,
        y: 100,
        type: "link",
        path: "/Applications"
      }
    ]
  },
  mac: {
    target: "dmg",
    category: "public.app-category.productivity",
  },
  win: {
    target: "nsis"
  },
  linux: {
    target: "AppImage",
    category: "Office"
  },
  fileAssociations: [{
    ext: "drop",
    name: "Droplet",
    description: "Droplet file",
    role: "Editor",
    icon: "build_res/drop.icns"
  }]
};

// When building a snapshot for testing, we don't need to
// include signing and notarizing, as that takes an annoying
// amount of time.
if (process.env.SNAPSHOT !== "true") {
  console.log("\n💧Droplet: packaging RELEASE version...\n");

  config = {
    ...config,

    // Signing
    mac: {
      ...config.mac,
      hardenedRuntime: true,
      entitlements: "build_res/entitlements.mac.plist",
      entitlementsInherit: "build_res/entitlements.mac.plist",
      gatekeeperAssess: false
    },

    // Notarizing
    afterSign: "scripts/notarize.js",
  }
} else {
  console.log("\n💧Droplet: packaging SNAPSHOT version...\n");
}

module.exports = config;

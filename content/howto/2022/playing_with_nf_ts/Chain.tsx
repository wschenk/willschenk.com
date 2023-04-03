interface BasicChainInformation {
  urls: string[];
  name: string;
}

export const CHAINS: { [chainId: number]: BasicChainInformation } = {
  1: {
    urls: ["https://cloudflare-eth.com"].filter((url) => url !== undefined),
    name: "Mainnet",
  },
};

export const URLS: { [chainId: number]: string[] } = Object.keys(
  CHAINS
).reduce<{ [chainId: number]: string[] }>((accumulator, chainId) => {
  const validURLs: string[] = CHAINS[Number(chainId)].urls;

  if (validURLs.length) {
    accumulator[Number(chainId)] = validURLs;
  }

  return accumulator;
}, {});

// @ts-ignore
export default function Chain({ chainId }) {
  const name = chainId ? CHAINS[chainId]?.name : undefined;

  return <>{name}</>;
}
